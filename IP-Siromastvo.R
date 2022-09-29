require(openxlsx)
require(plotly)
require(tidyverse)
require(ggplot2)
require(ggthemes)
siromastvo<- read.xlsx("IP-Siromastvo.xlsx")

siromastvo<- siromastvo %>% gather(God,Vrednost,5:12)
siromastvo$Starost[siromastvo$Starost== ' Ukupno '] <- "Ukupno stanovništvo"

ggplot(siromastvo %>% filter(trimws(Starost) %in% c("Ukupno stanovništvo", "65 i više godina","75 i više godina"))) + geom_bar(stat = "identity",aes(x=God, y=Vrednost, fill=Geo))
siromastvo$Starost <- as.factor(siromastvo$Starost)
siromastvo$Starost<- relevel(siromastvo$Starost,"Ukupno stanovništvo")

IPplotgg<- ggplot(
  siromastvo %>%
    filter(
      trimws(Starost) %in% c("Ukupno stanovništvo", "65-74 godina","75 i više godina"),
      Medijana == 60,
      Pol != "ukupno"
    ),
  aes(
    x = God,
    y = Vrednost,
    group = Pol,
    col = Pol,
    text = sprintf(
      "%s - %s. god.<br><b>%s%% </b> %s %s ",
      Geo,
      God,
      format(Vrednost, decimal.mark = ","),
      ifelse(Pol == "muškarci", "muškaraca", "žena"),
      ifelse(Starost == "Ukupno stanovništvo", "", "starijih od 65 godina")
    )
  )
) + scale_color_manual(values = c("#377eb8", "#e41a1c"))+
  scale_y_continuous(
  labels = function(x)
    paste0(x, "%")
)  + geom_line() + facet_grid(Geo ~ Starost) + ylab("Stopa rizika od siromaštva") + theme_minimal() +
  xlab(NULL)


IPplotly <- ggplotly(IPplotgg, tooltip = "text")
IPplotly


