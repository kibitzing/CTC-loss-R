#' CTC_loss() Function
#'
#' This function calculate CTC loss.
#' @param output_prob: output log probability matrix of model, b: ground truth label, blank_idx: index of blank token
#' @keywords ctcloss
#' @return CTC loss
#' @export
CTC_loss <- function(output_prob, label, blank_idx){

  output_length = dim(output_prob)[1]
  output_dim = dim(output_prob)[2]
  label_length = length(label)

  # matrix to save probability
  pmat = matrix(0, 2*label_length+1, output_length)
  for (i in 1: (2 * label_length + 1)){
    mod_of_i = i %% 2
    # blank
    if (mod_of_i == 1){
      # blank index assumed that starts with 0
      row_value = output_prob[, blank_idx + 1]
    } else { # non-blank
      j = as.integer(i %/% 2)
      # label index assumed that starts with 0
      row_value = output_prob[, label[j] + 1]
    }
    pmat[i, ] = row_value
  }
  # matrix for memoization
  memo_array = matrix(-Inf, 2*label_length+1, output_length)

  # Initialization
  memo_array[1, 1] = pmat[1, 1]
  memo_array[2, 1] = pmat[2, 1]

  for (time_pos in 2: output_length){
    for (script_pos in 1: (2*label_length+1)){
      # alpha 1: from same character, previous time step
      alpha1 = memo_array[script_pos, time_pos - 1]

      # alpha 2: from previous character, previous time step
      if (script_pos > 1){
        alpha2 = memo_array[script_pos - 1, time_pos - 1]
      } else {
        alpha2 = -Inf
      }

      # alpha 3: from previous previous character, previous time step
      if (script_pos %% 2 == 1){ # when current char is blank
        alpha3 = -Inf
      } else if (script_pos < 4){ # no previous previous character
        alpha3 = -Inf
      } else if (label[script_pos %/% 2] == label[script_pos %/% 2 - 1]){
        # pp char is same character
        alpha3 = -Inf
      } else {
        alpha3 = memo_array[script_pos - 2, time_pos - 1]
      }

      alpha = log(sum(exp(alpha1), exp(alpha2), exp(alpha3)))
      cur_path_prob = alpha + pmat[script_pos, time_pos]
      memo_array[script_pos, time_pos] = cur_path_prob
    }
  }

  last_step_non_blank = memo_array[2 * label_length, output_length]
  last_step_blank = memo_array[2 * label_length + 1, output_length]

  loss = log(sum(exp(last_step_non_blank), exp(last_step_blank)))

  return(-loss)
}
