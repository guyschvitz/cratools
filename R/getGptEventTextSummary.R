#' Summarise long text vectors using chunked GPT API calls and final aggregation
#'
#' @param event.text.vec Character vector of input text entries (e.g., event descriptions)
#' @param token Character. API authentication token
#' @param base.url Character. API base URL
#' @param model Character. Model name (e.g., "gpt-4o")
#' @param api.type Character. API type parameter for jrcgpt::getGptApiResponse
#' @param max.tokens Integer. Approximate max tokens per chunk **input** (default 100000)
#' @param response.max.tokens Integer. Max tokens for each GPT **response** (default 2048)
#' @param max.retries Integer. Maximum number of retry attempts for failed API calls (default 3)
#' @param retry.delay Numeric. Delay in seconds between retry attempts (default 1)
#' @param system.prompt1 Character. System prompt for first (chunk) pass. Default provided if NULL
#' @param user.prompt1.intro Character. Intro text prepended to each chunk's user prompt. Default if NULL
#' @param system.prompt2 Character. System prompt for final aggregation pass. Default provided if NULL
#' @param user.prompt2.intro Character. Intro text prepended to the final user prompt. Default if NULL
#'
#' @return List with:
#'   - intermediate.summaries (character vector of per-chunk summaries)
#'   - final.summary (character scalar with final aggregated summary)
#' @importFrom purrr map_chr map_dbl
#' @importFrom tokenizers tokenize_words
#' @export
getGptEventTextSummary <- function(
    event.text.vec,
    token,
    base.url,
    model,
    api.type,
    max.tokens = 100000,
    response.max.tokens = 2048,
    max.retries = 3,
    retry.delay = 1,
    system.prompt1 = NULL,
    user.prompt1.intro = NULL,
    system.prompt2 = NULL,
    user.prompt2.intro = NULL
) {
  
  # ---- Defaults ----
  if (is.null(system.prompt1)) {
    system.prompt1 <- paste(
      "You are a conflict analyst. Write short, factual paragraphs that summarise the key",
      "developments in the provided event descriptions.",
      "Please highlighting the following attributes in your summary (if available):",
      "(1) most common or significant events (2) main locations. (3) main actors involved.",
      "(4) political context (e.g. political events leading to protests or main demands of protesters).",
      "Avoid redundancy, and keep to 1-3 sentences. No conclusions."
    )
  }
  if (is.null(user.prompt1.intro)) {
    user.prompt1.intro <- "Summarise the following events into a concise paragraph according to the instructions provided."
  }
  if (is.null(system.prompt2)) {
    system.prompt2 <- paste(
      "You are a conflict analyst. Merge multiple event summaries into a single short summary.",
      "Remove duplicates, merge overlaps and keep to one short paragraph of 1-3 sentences. No introductions or conclusions."
    )
  }
  if (is.null(user.prompt2.intro)) {
    user.prompt2.intro <- "Combine and streamline these paragraphs into a single cohesive summary:"
  }
  
  # ---- Validation ----
  if (!is.character(event.text.vec) || length(event.text.vec) == 0) {
    stop("'event.text.vec' must be a non-empty character vector")
  }
  if (!is.character(token) || length(token) != 1 || nchar(token) == 0) {
    stop("'token' must be a non-empty character string")
  }
  if (!is.character(base.url) || length(base.url) != 1 || nchar(base.url) == 0) {
    stop("'base.url' must be a non-empty character string")
  }
  if (!is.character(model) || length(model) != 1 || nchar(model) == 0) {
    stop("'model' must be a non-empty character string")
  }
  if (!is.character(api.type) || length(api.type) != 1 || nchar(api.type) == 0) {
    stop("'api.type' must be a non-empty character string")
  }
  if (!is.numeric(max.tokens) || max.tokens <= 0) {
    stop("'max.tokens' must be a positive number")
  }
  if (!is.numeric(response.max.tokens) || response.max.tokens <= 0) {
    stop("'response.max.tokens' must be a positive number")
  }
  if (!is.numeric(max.retries) || max.retries < 0) {
    stop("'max.retries' must be a non-negative integer")
  }
  if (!is.numeric(retry.delay) || retry.delay < 0) {
    stop("'retry.delay' must be a non-negative number")
  }
  
  # ---- Compute chunking using tokenizers-based estimate ----
  # Calculate actual token counts for each entry
  entry.token.counts <- purrr::map_dbl(event.text.vec, function(entry) {
    word.count <- length(tokenizers::tokenize_words(entry)[[1]])
    # Use multiplier to approximate GPT tokens (GPT tokens are often shorter than words)
    return(word.count * 1.3)
  })
  
  # Add buffer for prompt overhead
  prompt.overhead <- nchar(user.prompt1.intro) + nchar(system.prompt1)
  effective.max.tokens <- max.tokens - prompt.overhead
  
  # Create chunks based on cumulative token counts
  chunked.text <- list()
  current.chunk <- character(0)
  current.tokens <- 0
  
  for (i in seq_along(event.text.vec)) {
    entry.tokens <- entry.token.counts[i]
    
    # If adding this entry would exceed the limit, start a new chunk
    if (current.tokens + entry.tokens > effective.max.tokens && length(current.chunk) > 0) {
      chunked.text <- append(chunked.text, list(current.chunk))
      current.chunk <- character(0)
      current.tokens <- 0
    }
    
    # Add the current entry to the chunk
    current.chunk <- c(current.chunk, event.text.vec[i])
    current.tokens <- current.tokens + entry.tokens
  }
  
  # Add the last chunk if it has content
  if (length(current.chunk) > 0) {
    chunked.text <- append(chunked.text, list(current.chunk))
  }
  
  # ---- First-pass summaries ----
  intermediate.summaries <- purrr::map_chr(
    .x = chunked.text,
    .f = \(txt.chunk) {
      user.prompt1 <- paste(user.prompt1.intro, paste(txt.chunk, collapse = "\n"), sep = "\n\n")
      messages <- list(
        list(role = "system", content = system.prompt1),
        list(role = "user",   content = user.prompt1)
      )
      
      resp.ls <- jrcgpt::getGptApiResponse(
        token = token,
        base.url = base.url,
        model = model,
        api.type = api.type,
        messages = messages,
        max.tokens = response.max.tokens,
        temperature = 0,
        max.retries = max.retries,
        retry.delay = retry.delay
      )
      
      resp.txt <- getGptResponseText(gpt.response = resp.ls, endpoint.type = "completions")
      
      # Validate response
      if (!is.character(resp.txt) || length(resp.txt) == 0) {
        stop("API returned invalid or empty response")
      }
      
      return(resp.txt)
    }
  )
  
  # ---- Final summary (only if multiple chunks) ----
  if (length(intermediate.summaries) > 1) {
    user.prompt2 <- paste(user.prompt2.intro, paste(intermediate.summaries, collapse = "\n\n"), sep = "\n\n")
    final.messages <- list(
      list(role = "system", content = system.prompt2),
      list(role = "user",   content = user.prompt2)
    )
    
    resp.ls <- jrcgpt::getGptApiResponse(
      token = token,
      base.url = base.url,
      model = model,
      api.type = api.type,
      messages = final.messages,
      max.tokens = response.max.tokens,
      temperature = 0,
      max.retries = max.retries,
      retry.delay = retry.delay
    )
    
    final.summary <- getGptResponseText(gpt.response = resp.ls, endpoint.type = "completions")
    
    # Validate response
    if (!is.character(final.summary) || length(final.summary) == 0) {
      stop("API returned invalid or empty response")
    }
  } else {
    # If only one chunk, just use that summary directly
    final.summary <- intermediate.summaries[[1]]
  }
  
  return(list(
    intermediate.summaries = intermediate.summaries,
    final.summary = final.summary
  ))
}
