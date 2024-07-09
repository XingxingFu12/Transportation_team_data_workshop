library(ggplot2)
library(ggstats)
library(dplyr)
library(viridis)
library(HH)
library(forcats)
library(scales)

Distribution$City <- factor(Distribution$City, levels=c("Utrecht", 
                                                        "Rotterdam",
                                                        "Total"))

Distribution$Mode <- factor(Distribution$Mode, levels= c("Walking", 
                                                         "Bicycle/e-bike",	
                                                         "Moped/motorcycle", 
                                                         "Car as a driver",
                                                         "Car as a passenger", 
                                                         "Bus", 
                                                         "Tram",	
                                                         "Metro", 
                                                         "Train",	
                                                         "Taxi/ride-hailing"))
Distribution$Frequency <- factor(Distribution$Frequency, levels = c("(Almost) Never",
                                                                    "A few times per year",
                                                                    "Once a month",
                                                                    "A few times a month",
                                                                    "Once a week",
                                                                    "A few times a week",
                                                                    "(Almost) Daily"))

Distribution$Mode <- fct_rev(Distribution$Mode)

df <- ggplot(Distribution, aes(fill=Frequency, x=Percentage, y=Mode)) +
  geom_bar(position= position_likert(reverse=FALSE), stat="identity", alpha=0.7, width =0.7) +
  geom_vline(linetype = "longdash", xintercept = 0, alpha=0.5, linewidth = 0.5) +
  scale_x_continuous(labels = scales::percent, limits = c(-1,1)) +
  facet_wrap(~City, nrow=3) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  guides(fill = guide_legend(reverse = TRUE)) +
  theme_bw()

ggsave("Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Modality styles & travel contexts/Analysis_revision/Distribution of mode use frequency.png",
       df, bg='transparent', width = 24, height = 18, units = "cm")

__________________________________________________________________________________________________________
CD$City <- factor(CD$City, levels=c("Utrecht", "Rotterdam", "Total"))

CD$Pattern <- factor(CD$"Modality style", levels = c("Multimodal",
                                                     "Drive mostly",
                                                     "Active + Train", 
                                                     "PT multimodal", 
                                                     "Moped/motorcycle multimodal"))

CD$Percent <- percent(CD$Percentage, accuracy=0.01)

cd<- ggplot(CD, aes(x= Pattern, fill=Pattern, y=Percentage)) +
  geom_bar(position="dodge", stat="identity") +
  geom_text(aes(label=Percent), vjust = -1, size = 3) +
  scale_y_continuous(labels = scales::percent, limits = c(0,0.5)) +
  scale_fill_viridis(discrete=TRUE, direction=1) +
  facet_wrap(~City, nrow=3) +
  guides(fill = guide_legend(title = "Modality style")) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

ggsave("Library/CloudStorage/OneDrive-UniversiteitUtrecht/Phd/Modality styles & travel contexts/Analysis_revision/Distribution of modality styles.png",
       cd, bg='transparent', width = 20, height = 20, units = "cm")






