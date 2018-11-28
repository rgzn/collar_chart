library("tidyverse")
library("ggthemes")

marks_tbl <- read_csv("Overview_Collars.csv")
names(marks_tbl) <- gsub(" ", "_", names(marks_tbl))
names(marks_tbl) <- gsub("%", "percent_", names(marks_tbl))


n_herds <- dim(unique(marks_tbl['Herd']))[1]
years <- seq(min(marks_tbl[['Sheep_Year']]),max(marks_tbl[['Sheep_Year']]))

marks_tbl <- marks_tbl %>%
  mutate(N_est = round(Marks / percent_Marks))

ggplot(data = marks_tbl %>% filter(Sex == "Female")) + 
  geom_line(mapping = aes(x = Sheep_Year, y = Marks)) + 
  facet_wrap(~ Herd, ncol = 1 )

markedherds_tbl <- marks_tbl %>%
  filter(Sex == "Female") %>%
  group_by(Herd) %>%
  filter(all(years %in% Sheep_Year))

mean_N <- mean(marks_tbl$N_est, na.rm = T)

largeherds_tbl <- marks_tbl %>%
  filter(Sex == "Female") %>%
  group_by(Herd) %>%
  filter(max(N_est, na.rm = T) > mean_N)

smallherds_tbl <- marks_tbl %>%
  filter(Sex == "Female") %>%
  group_by(Herd) %>%
  filter(max(N_est, na.rm = T) <= mean_N)


ggplot(largeherds_tbl) + 
  geom_area(mapping = aes(x = Sheep_Year, y = N_est), alpha = 0.4) +
  geom_area(mapping = aes(x = Sheep_Year, y = Marks), fill = "blue", alpha = 0.4) +
  geom_area(mapping = aes(x = Sheep_Year, y = GPS), fill = "blue") +
  geom_line(mapping = aes(x = Sheep_Year, y = 0.3*N_est), color = "red", size = 1.0) +
  ylab("# of ewes") + 
  facet_wrap(~ Herd, ncol = 2 )  

nmarks_tbl <- marks_tbl %>% select(Sheep_Year, Herd, Sex, N_est, Marks, GPS, VHF)
target_mark_fraction = 0.3
nmarks_tbl <- nmarks_tbl %>% mutate(Mark_Target = as.integer(floor(target_mark_fraction * N_est)) )
nmarks_long_tbl <- nmarks_tbl %>% 
  gather(count_type, n, N_est:Mark_Target) %>%
  mutate(count_type = gsub("N_est", "Est_Total", count_type))

plot_tbl <- nmarks_long_tbl %>% 
  filter(Sex == "Female") %>%
  filter(count_type != "VHF") %>%
  group_by(Herd) %>%
  filter(max(n, na.rm = T) > mean_N)

ggplot() +
  geom_area(data = plot_tbl %>% filter(count_type != "Mark_Target"),
            aes(x=Sheep_Year, y=n, fill=count_type), position = "identity", alpha = 0.8) + 
  geom_line(data = plot_tbl %>% filter(count_type == "Mark_Target"), 
            aes(x=Sheep_Year, y=n, col="Mark_Target"), size = 1) +
  scale_colour_manual(name = "", values = c("Mark_Target" = "coral3")) +
  scale_fill_manual(name = "Counts", values = 
                      c("Est_Total" = "grey",
                        "Marks" = "cornflowerblue",
                        "GPS" = "blue4")) + 
  scale_x_continuous(expand=c(0,0), limits=c(min(years), max(years))) +
  scale_y_continuous(expand=c(0,0), limits=c(0, max(plot_tbl$n) + 2)) +
  ylab("# of ewes") +
  ggtitle("Sierra Bighorn Marks Relative to Population (large herds only)") + 
  theme(axis.title.x=element_text(size=10, lineheight=.9, face="bold")) + 
  theme(axis.title.y=element_text(size=10, lineheight=.9, face="bold")) + 
  facet_wrap(~ Herd, ncol=2)



## separations : older vs newer, older +not tiny vs newer, 

## percentage as one graph, N as another? 
