

```{r Summarise Best Performing By Type, message=FALSE, warning=FALSE, include=FALSE, paged.print=FALSE}

Property_Type_Performance <- resale_merge %>%
  mutate(growth_bin = ifelse(`Annual Capital Growth` < 0, "Less than 0% Per Annum",
                             ifelse(`Annual Capital Growth` > 0 & `Annual Capital Growth` < 0.025, "Between 0% and 2.5% Per Annum",
                                    ifelse(`Annual Capital Growth` > 0.025 & `Annual Capital Growth` < 0.05, "Between 2.5% and 5% Per Annum",
                                           ifelse(`Annual Capital Growth` > 0.05 & `Annual Capital Growth` < 0.075, "Between 5% and 7.5% Per Annum", "More than 7.5% Per Annum"))))) %>%
  group_by(Type, growth_bin) %>%
  summarise(n()) 



```




```{r Heatmap of Best Performing Buildings, eval=FALSE, fig.align=, fig.height=6, fig.width=10, include=FALSE}
# these are manually entered but in future should be calculated with code

row1 <- c("38%", "31%", "23%", "15%", "17%")
row2 <- c("35%", "29%", "29%", "39%", "30%")
row3 <- c("17%", "22%", "28%", "26%", "42%")
row4 <- c("4%", "10%", "11%", "15%", "10%")
row5 <- c("6%", "8%", "9%", "5%", "1%")

matrix <- as.data.frame(as.matrix(rbind(row1, row2, row3, row4, row5)))

colnames(matrix) <- c("High-Rise Apartments Buildings", "Mid-Rise Apartment Buildings", "Low-Rise Apartment Buildings", "Small Size Townhouse Complexes", "Medium Sized Townhouses Complexes")

rownames(matrix) <- c("Less than 0% per annum", "Between 0% and 2.5% per annum", "Between 2.5% and 5% per annum", "Between 5% and 7.5% per annum", "More than 7.5% per annum")

library(kableExtra)

kable(matrix) %>%
  kable_styling()








library(reshape2)

melted.matrix <- melt(matrix)

ggplot(data = melted.matrix, aes(x=Var2, y=Var1, fill=value)) + 
  geom_tile(color = "black") +
  theme(axis.text.x = element_text(angle = 25, hjust = 1),
        panel.background = element_blank(),
        axis.line = element_line(colour = "black")) +
  scale_fill_continuous(name = "Proportion of Sales per Type (%)") +
  labs(title = "Annual Capital Growth Matrix",
       x = NULL,
       y = NULL,
       caption = "Average Annual Capital Growth by Property Type and Complex Size by Six Month Periods") 


```