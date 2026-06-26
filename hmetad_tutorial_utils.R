plot_joint_response <- function(data, ..., K=4) {
  terms <- names(rlang::enquos(..., .named=TRUE))
  
  p <- data |>
    mutate(joint_response=factor(as.integer(ifelse(response,
                                                   confidence + K,
                                                   K + 1 - confidence)),
                                 levels=1:(K*2)),
           response=factor(response),
           confidence=factor(confidence)) |>
    group_by(stimulus, ...) |>
    count(joint_response) |>
    mutate(p=n/sum(n),
           stimulus=factor(stimulus)) |>
    ggplot(aes(x=joint_response, y=p, group=stimulus, fill=stimulus)) +
    geom_col(position=position_dodge(.925)) +
    scale_x_discrete(labels=c('Confident\n"0"', rep('', K-2), 'Guess\n"0"',
                              'Guess\n"1"', rep('', K-2), 'Confident\n"1"')) +
    scale_fill_discrete('Stimulus') +
    theme_classic(18, paper=alpha('white', 0)) +
    theme(axis.title=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())

  if (length(terms) == 0) {
    p
  } else {
    p + facet_grid(as.formula(paste0(str_c(terms, sep='+'), ' ~ .')),
                   labeller=label_both)
  }
}

plot_confidence <- function(data, ..., K=4) {
  terms <- names(rlang::enquos(..., .named=TRUE))
  
  p <- data |>
    mutate(correct=factor(correct, levels=0:1),
           confidence=factor(confidence, levels=1:K)) |>
    group_by(correct, ...) |>
    count(confidence) |>
    mutate(p=n/sum(n)) |>
    ggplot(aes(x=confidence, y=p, group=correct, fill=correct)) +
    geom_col(position=position_dodge(.925)) +
    xlab('Confidence') +
    scale_fill_discrete('Correct') +
    theme_classic(18, paper=alpha('white', 0)) +
    theme(axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          axis.line.y=element_blank())

  if (length(terms) == 0) {
    p
  } else {
    p + facet_grid(as.formula(paste0(str_c(terms, sep='+'), ' ~ .')),
                   labeller=label_both)
  }
}

plot_roc1 <- function(data, ..., K=4) {
  terms <- names(rlang::enquos(..., .named=TRUE))
  
  data <- data |>
    mutate(joint_response=joint_response(response, confidence, K)) |>
    group_by(stimulus, ...) |>
    count(joint_response) |>
    mutate(p=n/sum(n)) |>
    pivot_wider(names_from=stimulus, values_from=n:p) |>
    mutate(p_hit=1-cumsum(p_1),
           p_fa=1-cumsum(p_0)) |>
    complete(tibble(joint_response=2*K, p_hit=1, p_fa=1))

  if (length(terms)==0) {
    p <- ggplot(data, aes(x=p_fa, y=p_hit))
  } else {
    p <- ggplot(data, aes(x=p_fa, y=p_hit,
                          color=!!rlang::data_sym(first(terms)),
                          group=interaction(!!!rlang::data_syms(terms))))
  }

  p + geom_abline(intercept=0, slope=1, linetype='dashed') +
    geom_line() +
    geom_point() +
    coord_fixed(xlim=c(0, 1), ylim=c(0, 1)) +
    xlab('P(Hit)') + ylab('P(False Alarm)') +
    theme_bw(18, paper=alpha('white', 0)) +
    theme(panel.grid=element_blank())
}

plot_roc2 <- function(data, ..., K=4, by_response=FALSE) {
  terms <- names(rlang::enquos(..., .named=TRUE))
  p <- NULL

  data <- data |>
    group_by(correct, ...) |>
    count(confidence) |>
    mutate(p=n/sum(n)) |>
    pivot_wider(names_from=correct, values_from=n:p) |>
    mutate(p_hit2=1-cumsum(p_1),
           p_fa2=1-cumsum(p_0)) |>
    complete(tibble(confidence=1, p_hit2=1, p_fa2=1))

  if (length(terms) == 0) {
    p <- ggplot(data, aes(x=p_fa2, y=p_hit2))
  } else {
    p <- ggplot(data, aes(x=p_fa2, y=p_hit2,
                          color=!!rlang::data_sym(first(terms)),
                          group=interaction(!!!rlang::data_syms(terms))))
  }

  p + geom_point() +
    geom_abline(intercept=0, slope=1, linetype='dashed') +
    geom_line() +
    coord_fixed(xlim=c(0, 1), ylim=c(0, 1), expand=FALSE) +
    xlab('P(Confidence ≥ k | Incorrect)') + ylab('P(Confidence ≥ k | Correct)') +
    theme_bw(18, paper=alpha('white', 0)) +
    theme(panel.grid=element_blank())
}

