## ---- message = FALSE, warning = FALSE, echo = FALSE--------------------------
library(igcop)
library(ggplot2)
select_alpha <- c(0.1, 0.5, 1, 3, 9)
select_eta <- c(0.1, 2, 20, 200)
alpha_title <- paste("alpha =", select_alpha)
eta_title <- paste("eta =", select_eta)

## ---- echo = FALSE, fig.width = 7, fig.height = 2, fig.align = "center"-------
dat1 <- expand.grid(alpha = select_alpha, 
                   x = seq(0, 50, length.out = 1000))
dat1$alpha_title <- paste("alpha =", dat1$alpha)
dat1$psi <- igcop:::igl_gen(dat1$x, dat1$alpha)
ggplot(dat1, aes(x, psi)) +
  facet_wrap(~ alpha_title, nrow = 1) +
  geom_line() +
  theme_bw() +
  ylab(expression(psi[alpha](x))) +
  theme(axis.title.y = element_text(angle = 0))

## ---- echo = FALSE, fig.width = 7, fig.height = 4, fig.align = "center"-------
dat2 <- expand.grid(alpha = select_alpha,
                    eta = select_eta,
                    x = seq(0, 2.5, length.out = 1000))
dat2$alpha_title <- paste("alpha =", dat2$alpha)
dat2$eta_title <- paste("eta =", dat2$eta)
dat2H <- dat2
dat2e <- dat2
dat2H$eval <- igcop:::interp_gen(dat2$x, eta = dat2$eta, alpha = dat2$alpha)
dat2H$fun <- "H"
dat2e$eval <- exp(-dat2$x)
dat2e$fun <- "e"
dat2 <- rbind(dat2H, dat2e)

# df <- expand_grid(nesting(alpha = select_alpha,
#                     alpha_title = alpha_title),
#             nesting(eta = select_eta,
#                     eta_title = eta_title),
#             x = seq(0, 2.5, length.out = 1000)) %>%
#   mutate(H = pmap_dbl(list(x, eta, alpha), igcop:::interp_gen),
#          e = exp(-x)) %>%
#   pivot_longer(cols = c("H", "e"), names_to = "fun", values_to = "eval")
ggplot(dat2, aes(x, eval)) +
  facet_grid(eta_title ~ alpha_title) +
  geom_line(aes(group = fun, linetype = fun, alpha = fun)) +
  ylab(expression(H[alpha](x*";"~eta))) +
  guides(linetype = "none", alpha = "none") + 
  scale_alpha_manual(values = c(0.5, 1)) +
  scale_linetype_manual(values = c("dotted", "solid")) +
  scale_x_continuous(breaks = 0:2) +
  theme_bw() +
  theme(axis.title.y = element_text(angle = 0))

