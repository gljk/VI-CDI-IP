require(openxlsx)
require(plotly)
require(tidyverse)
require(ggplot2)
require(ggthemes)
siromastvo<- read.xlsx("IP-Siromastvo.xlsx")

siromastvo<- siromastvo %>% gather(God,Vrednost,5:12)
siromastvo$Starost[siromastvo$Starost== ' Ukupno '] <- "Ukupno stanovništvo"

ggplot(siromastvo %>% filter(trimws(Starost) %in% c("Ukupno stanovništvo", "65 i više godina"))) + geom_bar(stat = "identity",aes(x=God, y=Vrednost, fill=Geo))

IPplotgg<- ggplot(
  siromastvo %>%
    filter(
      trimws(Starost) %in% c("Ukupno stanovništvo", "65 i više godina"),
      Medijana == 50,
      Pol != "ukupno"
    ),
  aes(
    x = God,
    y = Vrednost,
    group = Pol,
    col = Pol,
    text=sprintf("%s - %s. god.<br><b>%s%% </b>siromašnih %s %s ",Geo,God,Vrednost,ifelse(Pol=="muškarci", "muškaraca", "žena"), ifelse(Starost=="Ukupno stanovništvo", "", "starijih od 65 godina"))
  )
)+ scale_color_manual(values = c("#377eb8", "#e41a1c"))+ geom_line() + facet_grid(Geo ~ Starost) + ylab("Udeo siromašnih")+ theme_minimal()+xlab(NULL)


IPplotly <- ggplotly(IPplotgg, tooltip="text")
IPplotly


