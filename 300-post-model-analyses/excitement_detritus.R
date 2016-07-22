# linear -- find comments in the prior X seconds, count as  (X - t) / X intensity

linear.excitement <- function(X) {
  tt <- thread.timing %>% 
    left_join(select(thread.timing, id, root.id, secs.since.thread), by="root.id") %>%
    filter(secs.since.thread.x - secs.since.thread.y >= 0  & 
             secs.since.thread.x - secs.since.thread.y < X &
             id.x != id.y) %>%
    mutate(secs.ago = secs.since.thread.x - secs.since.thread.y) %>%
    group_by(id.x) %>%
    summarize(linear.X = sum((X - secs.ago) / X)) %>%
    right_join(thread.timing, by=c("id.x" = "id")) %>%
    select(id.x, linear.X) %>%
    transmute(id=id.x, 
              excitement = ifelse(is.na(linear.X), 0, linear.X))
  return(tt)
}

linear.15 <- linear.excitement(900)
linear.30 <- linear.excitement(1800)
linear.60 <- linear.excitement(3600)
linear.120 <- linear.excitement(7200)
linear.180 <- linear.excitement(180 * 60)
linear.240 <- linear.excitement(240 * 60)
linear.set <- 
  merge(linear.15, linear.30, by="id", suffixes=c("", ".30")) %>%
  merge(linear.60, by="id", suffixes=c("",".60")) %>%
  merge(linear.120, by="id", suffixes=c("",".120")) %>%
  merge(linear.180, by="id", suffixes=c("",".180")) %>%
  merge(linear.240, by="id", suffixes=c(".15",".240")) 

th.doc.topics <- left_join(th.doc.topics, linear.set, by="id")

gg <- 
  th.doc.topics %>%
  filter(!missing.parent & !is.first.comment) %>%
  select(excitement.60, excitement.120, excitement.180, excitement.240) %>%
  gather(window.minutes, excitement) %>%
  ggplot(aes(excitement, colour=window.minutes)) +
  geom_density(size=1) +
  coord_cartesian(xlim=c(0,5), ylim=c(0,1.2)) +
  theme(text=element_text(size=16))
ggsave(gg, file="outputs/excitement_linear_decay_densities_hours.png")

# exponential decay
# keep a running total score
# each comment, reduce running total multiplying by decay.rate ^ seconds.since.prior

exp.excitement <- function(half.life.sec) {
  alpha <- exp(log(0.5) / half.life.sec)
  tt <- thread.timing %>% 
    left_join(select(thread.timing, id, root.id, secs.since.thread), by="root.id") %>%
    filter(secs.since.thread.x >= secs.since.thread.y  &
             id.x != id.y) %>%
    mutate(secs.ago = secs.since.thread.x - secs.since.thread.y) %>%
    group_by(id.x) %>%
    summarize(exp.X = sum(alpha ^ secs.ago)) %>%
    right_join(thread.timing, by=c("id.x" = "id")) %>%
    select(id.x, exp.X) %>%
    transmute(id=id.x, 
              exp.excite = ifelse(is.na(exp.X), 0, exp.X))
  return(tt)
}
exp.1 <- exp.excitement(60)
exp.2 <- exp.excitement(120)
exp.5 <- exp.excitement(300)
exp.10 <- exp.excitement(600)
exp.20 <- exp.excitement(1200)
exp.30 <- exp.excitement(1800)
exp.60 <- exp.excitement(3600)
exp.set <- 
  merge(exp.5, exp.10, by="id", suffixes=c("", ".10")) %>%
  merge(exp.20, by="id", suffixes=c("",".20")) %>%
  merge(exp.30, by="id", suffixes=c("",".30")) %>%
  merge(exp.60, by="id", suffixes=c(".5",".60")) 

th.doc.topics <- left_join(th.doc.topics, exp.set, by="id")