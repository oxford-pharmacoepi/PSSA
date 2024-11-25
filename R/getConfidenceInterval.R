# CI
getConfidenceInterval <- function(table, nsr, confidenceInterval = 95){

  confidenceIntervalLevel <- (100-confidenceInterval)/200
  counts <- tibble::tibble(
    index_first = table |> dplyr::pull("index_first") |> sum(),
    marker_first = table |> dplyr::pull("marker_first") |> sum()
  )

   if (counts$index_first == 0){
    counts$index_first <-  0.5
    counts$lower_csr_ci <- stats::qbeta(confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upper_csr_ci <- stats::qbeta(1-confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)

    counts$lower_csr_ci <- counts$lower_csr_ci/(1-counts$lower_csr_ci)
    counts$upper_csr_ci <- counts$upper_csr_ci/(1-counts$upper_csr_ci)

    counts$lower_asr_ci <- counts$lower_csr_ci/nsr
    counts$upper_asr_ci <- counts$upper_csr_ci/nsr

  } else if (counts$marker_first == 0){
    counts$lower_csr_ci <- Inf
    counts$upper_csr_ci <- Inf

    counts$lower_asr_ci <- Inf
    counts$upper_asr_ci <- Inf

  } else {
    counts$lower_csr_ci <- stats::qbeta(confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)
    counts$upper_csr_ci <- stats::qbeta(1-confidenceIntervalLevel, counts$index_first + 0.5, counts$marker_first + 0.5)

    counts$lower_csr_ci <- counts$lower_csr_ci/(1-counts$lower_csr_ci)
    counts$upper_csr_ci <- counts$upper_csr_ci/(1-counts$upper_csr_ci)

    counts$lower_asr_ci <- counts$lower_csr_ci/nsr
    counts$upper_asr_ci <- counts$upper_csr_ci/nsr
  }
  return(counts)
}
