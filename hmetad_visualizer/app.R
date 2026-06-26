library(bslib)
library(shiny)
library(distributional)
library(ggdist)
library(ggplot2)
library(tidyr)
library(dplyr)
library(purrr)
library(paletteer)
library(patchwork)

#' copied from hmetad to reduce dependencies
to_signed <- function(x) ifelse(x, 1, -1)

#' copied from hmetad to reduce dependencies
metad_pmf <- function(stimulus, dprime, c,
                      meta_dprime, meta_c,
                      meta_c2_0, meta_c2_1,
                      lcdf = function(x, mu) pnorm(x, mean = mu, log.p = TRUE),
                      lccdf = function(x, mu) pnorm(x, mean = mu, log.p = TRUE, lower.tail = FALSE),
                      log = FALSE) {
  if (!is.numeric(stimulus) || stimulus < 0 || stimulus > 1 ||
        (stimulus > 0 && stimulus < 1)) {
    stop(paste0("Stimulus should be `0` or `1`, but is ", stimulus))
  }
  if (!all(
    length(dprime) == 1, length(c) == 1, length(meta_dprime) == 1,
    is.numeric(dprime), is.numeric(c), is.numeric(meta_dprime)
  )) {
    stop("Error: `dprime`, `c`, and `meta_dprime` must be single numbers.")
  }
  if (!is.numeric(meta_c2_0) || !is.numeric(meta_c2_1) ||
        length(meta_c2_0) != length(meta_c2_1) ||
        !all(diff(c(meta_c, meta_c2_0)) < 0) ||
          !all(diff(c(meta_c, meta_c2_1)) > 0)) {
    stop("Error: `meta_c2_0` and meta_c2_1` must be ordered vectors of the same length constrained by `meta_c`.")
  }

  # number of confidence levels
  K <- length(meta_c2_0) + 1

  # type-1 response probabilities
  lp_1 <- lccdf(c, to_signed(stimulus) * dprime / 2)
  lp_0 <- lcdf(c, to_signed(stimulus) * dprime / 2)

  # calculate normal cdfs (log scale)
  lp2_1 <- lccdf(c(meta_c, meta_c2_1), to_signed(stimulus) * meta_dprime / 2)
  lp2_0 <- lcdf(c(meta_c, meta_c2_0), to_signed(stimulus) * meta_dprime / 2)

  # response probabilities
  log_theta <- rep(0, 2 * K)
  for (k in 1:(K - 1)) {
    log_theta[K - k + 1] <- log(exp(lp2_0[k]) - exp(lp2_0[k + 1]))
    log_theta[K + k] <- log(exp(lp2_1[k]) - exp(lp2_1[k + 1]))
  }
  log_theta[1] <- lp2_0[K]
  log_theta[2 * K] <- lp2_1[K]

  # weight by P(response|stimulus) and normalize
  log_theta[1:K] <- log_theta[1:K] + lp_0 - lp2_0[1]
  log_theta[(K + 1):(2 * K)] <- log_theta[(K + 1):(2 * K)] + lp_1 - lp2_1[1]

  if (log) {
    log_theta
  } else {
    exp(log_theta)
  }
}


ALPHA <- .33
PALETTE <- paletteer_d("fishualize::Phractocephalus_hemioliopterus")[c(1, 3)]
DIST_COLOR <- paletteer_d("fishualize::Phractocephalus_hemioliopterus")[4]
DELTA_COLOR <- paletteer_d("fishualize::Phractocephalus_hemioliopterus")[2]

THEME_SDT <- function(xlab='Evidence', limits=c(-2.5, 2.5), base_size=18,
                      expand=expansion(mult=c(.1, .1))) {
  list(scale_x_continuous(xlab, limits=limits, breaks=seq(-4, 4, by=2), expand=expand),
       scale_color_manual('Stimulus', values=PALETTE),
       scale_fill_manual('Stimulus', values=PALETTE),
       scale_y_continuous(expand=c(0, 0)),
       theme_classic(base_size=base_size),
       theme(axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank(),
             axis.line.y=element_blank()))
}

ui <- page_sidebar(
  title="The meta-d' model of confidence ratings",
  sidebar=sidebar(
    position="right", width="33%",
    sliderInput("d", "Type 1 sensitivity (d')",
                min=0, max=5, value=1.5, step=.01),
    sliderInput("c", "Type 1 response bias (c)",
                min=-2, max=2, value=0, step=.01),
    sliderInput("metad", "Type 2 sensitivity (meta-d')",
                min=0, max=5, value=1.5, step=.01),
    sliderInput("metac2_0_1", "Type 2 response bias (meta-c2-0-1-diff)",
                min=0, max=1.5, value=.75, , step=.01),
    sliderInput("metac2_0_2", "Type 2 response bias (meta-c2-0-2-diff)",
                min=0, max=1.5, value=.75, step=.01),
    sliderInput("metac2_1_1", "Type 2 response bias (meta-c2-1-1-diff)",
                min=0, max=1.5, value=.75, step=.01),
    sliderInput("metac2_1_2", "Type 2 response bias (meta-c2-0-2-diff)",
                min=0, max=1.5, value=.75, step=.01)
    #, selectInput("metac_absolute", "Type 2 response bias", choices=c("Absolute", "Relative"))
  ),
  plotOutput('type1'),
  plotOutput('type2'),
  navset_pill(
    nav_panel('Joint Distribution', plotOutput('joint_response')),
    nav_panel('Pseudo Type 1 ROC/Type 2 ROC', plotOutput('roc'))
  )
)

server <- function(input, output) {
  ## set SDT distributions
  evidence <- reactive({
    tibble(stimulus=factor(0:1),
           d_prime=input$d, c=input$c, meta_d_prime=input$metad,
           mu1=c(-1, 1)*d_prime/2, sd=1,
           mu2=c(-1, 1)*meta_d_prime/2)
  })

  # set type 2 criteria
  c2_0 <- reactive({
    input$c - cumsum(c(input$metac2_0_1, input$metac2_0_2))
  })
  c2_1 <- reactive({
    c2_1 <- input$c + cumsum(c(input$metac2_1_1, input$metac2_1_2))
  })

  # joint type 1/type 2 response probabilities
  joint_prob <- reactive({
    K <- 3
    tibble(stimulus=rep(0:1, each=6),
           response=rep(0:1, each=K, times=2),
           confidence=rep(c(rev(seq_len(K)), seq_len(K)), times=2),
           p=c(metad_pmf(0, input$d, input$c, input$metad, input$c, c2_0(), c2_1()),
               metad_pmf(1, input$d, input$c, input$metad, input$c, c2_0(), c2_1()))) |>
      mutate(stimulus=factor(stimulus, levels=0:1),
             joint_response=factor(as.integer(ifelse(response,
                                                     confidence + K,
                                                     K + 1 - confidence)),
                                   levels=1:(2*K)),
             response=factor(response, levels=0:1),
             confidence=factor(confidence, levels=1:K))
  })
  
  output$type1 <- renderPlot({
    ggplot(evidence(), aes(xdist=dist_normal(mu1, sd))) +
      stat_slab(aes(fill=stimulus), color=NA, alpha=ALPHA, scale=.8, show.legend=FALSE) +
      geom_vline(aes(xintercept=c)) +
      THEME_SDT(xlab='Type 1 Evidence', expand=expansion())
  })

  output$type2 <- renderPlot({
    p.type2.0 <- ggplot(evidence(), aes(xdist=dist_truncated(dist_normal(mu2, sd), upper=c))) +
      stat_slab(aes(fill=stimulus), color=NA, alpha=ALPHA, scale=.8, show.legend=FALSE) +
      geom_vline(xintercept=input$c) +
      geom_vline(aes(xintercept=x), data=tibble(x=c2_0())) +
      THEME_SDT(xlab='Type 2 Evidence\n("0" Response)', limits=c(-2.5, input$c),
                expand=expansion(add=c(0, 0.01)))
    p.type2.1 <- ggplot(evidence(), aes(xdist=dist_truncated(dist_normal(mu2, sd), lower=c))) +
      stat_slab(aes(fill=stimulus), color=NA, alpha=ALPHA, scale=.8, show.legend=FALSE) +
      geom_vline(xintercept=input$c) +
      geom_vline(aes(xintercept=x), data=tibble(x=c2_1())) +
      THEME_SDT(xlab='Type 2 Evidence\n("1" Response)', limits=c(input$c, 2.5),
                expand=expansion(add=c(.01, 0)))

    offset <- (input$c+2.5) / 5
    
    (p.type2.0 | p.type2.1) +
      plot_layout(widths=c(offset, 1-offset))
  })

  output$joint_response <- renderPlot({
    ggplot(joint_prob(), aes(x=joint_response, y=p, group=stimulus, fill=stimulus)) +
      geom_col(position=position_dodge(.925)) +
      scale_x_discrete(labels=c('Confident\n"0"', '', 'Guess\n"0"',
                                'Guess\n"1"', '', 'Confident\n"1"')) +
      scale_fill_manual('Stimulus', values=PALETTE) +
      theme_classic(18) +
      theme(axis.title=element_blank(),
            axis.text.y=element_blank(),
            axis.ticks.y=element_blank(),
            axis.line.y=element_blank(),
            legend.position='bottom')
  })

  output$roc <- renderPlot({
    roc1 <- joint_prob() |>
      pivot_wider(names_from=stimulus, values_from=p, names_prefix='p_') |>
      mutate(p_hit=1-cumsum(p_1),
             p_fa=1-cumsum(p_0)) |>
      complete(tibble(p_hit=1, p_fa=1)) |>
      ggplot(aes(x=p_fa, y=p_hit)) +
      geom_abline(intercept=0, slope=1, linetype='dashed') +
      geom_line() +
      geom_point() +
      coord_fixed(xlim=c(0, 1), ylim=c(0, 1)) +
      xlab('P(Hit)') + ylab('P(False Alarm)') +
      theme_bw(18) +
      theme(panel.grid=element_blank())

    roc2 <- joint_prob() |>
      mutate(correct=as.integer(response==stimulus)) |>
      select(-joint_response, -stimulus) |>
      pivot_wider(names_from=correct, values_from=p, names_prefix='p_') |>
      group_by(response) |>
      arrange(confidence) |>
      mutate(p_1=p_1/sum(p_1),
             p_0=p_0/sum(p_0),
             p_hit2=1-cumsum(p_1),
             p_fa2=1-cumsum(p_0),
             response=factor(response, levels=0:1, labels=c('"0"', '"1"'))) |>
      complete(tibble(p_hit2=1, p_fa2=1)) |>
      ggplot(aes(x=p_fa2, y=p_hit2, color=response)) +
      geom_abline(intercept=0, slope=1, linetype='dashed') +
      geom_point() +
      geom_line() +
      coord_fixed(xlim=c(0, 1), ylim=c(0, 1), expand=FALSE) +
      xlab('P(Confidence ≥ k | Incorrect)') + ylab('P(Confidence ≥ k | Correct)') +
      theme_bw(18) +
      theme(panel.grid=element_blank())

    roc1 | roc2    
  })
}

shinyApp(ui = ui, server = server)

##  Use the following code to upload to shinyapps.io:
##
## library(rsconnect)
## deployApp(appName='hmetad')
