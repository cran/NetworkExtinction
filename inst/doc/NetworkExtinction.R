## ----setup, include = FALSE---------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>",
  warning = FALSE,
  cache = FALSE
)
knitr::opts_chunk$set(fig.path = file.path(getwd(), "vignettes/figures"))
knitr::opts_chunk$set(fig.width=6, fig.height=4, message = FALSE) 

## ---- eval=FALSE--------------------------------------------------------------
#  install.packages(NetworkExtinction)
#  library(NetworkExtinction)

## ---- eval=FALSE--------------------------------------------------------------
#  devtools::install_github("derek-corcoran-barrios/NetworkExtintion")

## ----Network, echo=FALSE, fig.cap= "Figure 1. Food-web to be contructed in R"----
knitr::include_graphics("toymodel_trophic-network5.jpg")

## ----Matrix, echo=FALSE, fig.cap= "Figure 2. Matrix representation of the food web"----
knitr::include_graphics("matrix.jpg")

## -----------------------------------------------------------------------------
a<- matrix(c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 1, 0, 0),nrow=10, ncol=10)

a

## ----library------------------------------------------------------------------
library(network)
net <- as.network(a, loops = TRUE)
net

## ----mostconnected1, eval=FALSE-----------------------------------------------
#  library(NetworkExtinction)
#  data("net")
#  SimulateExtinctions(Network = net, Method = "Mostconnected")

## ----mostconnected2, echo=FALSE, results='hide', fig.keep='all', message = FALSE----
library(NetworkExtinction)
data("net")
knitr::kable(
  SimulateExtinctions(Network = net, Method = "Mostconnected")[[1]],
  caption = "Table 1: The resulting dataframe of the SimulateExtinctions in Mostconnected method")

## ----mostconnected3, fig.cap="Figure 3. The graph shows the number of accumulated secondary extinctions that occur when removing species from the most to the least connected species", results='hide', fig.keep='all', message = FALSE----
data("More_Connected")
history <- SimulateExtinctions(Network = net, Method = "Mostconnected")
ExtinctionPlot(History = history[[1]], Variable = "AccSecExt")

## ---- echo=FALSE, results='hide', fig.keep='all', message = FALSE-------------
SimulateExtinctions(Network = net, Method = "Mostconnected")[[2]]

## ---- eval=FALSE--------------------------------------------------------------
#  data("net")
#  SimulateExtinctions(Network = net, Order = 1:8, Method = "Ordered")

## ---- echo=FALSE, results='hide', fig.keep='all', message = FALSE-------------
data("net")
knitr::kable(SimulateExtinctions(Network = net, Order = 1:8, Method = "Ordered"), caption = "Table 2: The resulting dataframe of the ExtinctionOrder function")

## ---- echo=FALSE, fig.cap= "Figure 4. The graph shows the number of accumulated secondary extinctions that occur when removing species in a custom order.", results='hide', fig.keep='all', message = FALSE----
data("net")
Order <- SimulateExtinctions(Network = net, Order = 1:8, Method = "Ordered")
ExtinctionPlot(History = Order[[1]], Variable = "AccSecExt")

## ---- eval = FALSE------------------------------------------------------------
#  data(net)
#  set.seed(707)
#  RandomExtinctions(Network= net, nsim= 100, SimNum = 8)

## ---- echo = FALSE, results='hide', fig.keep='all', message = FALSE-----------
data(net)
set.seed(707)
Test <- RandomExtinctions(Network= net, nsim= 100, SimNum = 8)
knitr::kable(Test[[1]], caption = "Table 3: The resulting dataframe of the RandomExtinctions function")

## ---- echo = FALSE, fig.cap= "Figure 5. The resulting graph of the RandomExtinctions function", results='hide', fig.keep='all', message = FALSE----
data(net)
set.seed(707)
Test <- RandomExtinctions(Network= net, nsim= 100, plot = TRUE, SimNum = 8)

## ----message=FALSE, warning=FALSE---------------------------------------------
data("net")
Comparison <- CompareExtinctions(Nullmodel = Test, Hypothesis = Order)

## ---- echo=FALSE, fig.cap= "Figure 6. The resulting graph of the CompareExtinctions function, where the dashed line shows the observed extinction history, and a solid line shows the expected value of secondary extinctions originated at random"----
Comparison

## ---- fig.cap= "Figure 7. Example of the use of the ExtinctionPlot function showing the accumulated secondary extinctions against number of extinctions"----
data(net)
ExtinctionPlot(History = Order[[1]])

## ---- fig.cap= "Figure 8. Another example of the use of the ExtinctionPlot function showing the number of links per species against number of extinctions"----
ExtinctionPlot(History = Order[[1]], Variable = "Link_density")

## ---- eval=FALSE--------------------------------------------------------------
#  data("chilean_intertidal")
#  DegreeDistribution(chilean_intertidal)

## ---- echo=FALSE--------------------------------------------------------------
data("chilean_intertidal")
Dist <- DegreeDistribution(chilean_intertidal)

## ---- echo = FALSE, fig.cap= "Figure 9: Fitted vs observed values of the degree distribution. The black line and points show the observed values, the red, green and blue lines show the fitted values for the Exponential, power law and trucated distribution, respectively"----
Dist$graph

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(Dist$models, caption = "Table 4: Model selection analysis")

## ---- results='hide', fig.keep='all', message = FALSE-------------------------
IS_0 <- SimulateExtinctions(Network = net, Order = 1:2, Method = "Ordered")[[1]]

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(IS_0 , caption = "Table 5: The resulting dataframe of the basic version of SimulateExtinctions")

## ---- results='hide', fig.keep='all', message = FALSE-------------------------
IS_0.7 <- SimulateExtinctions(Network = net, Order = 1:2, Method = "Ordered", IS = 0.7)[[1]]

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(IS_0.7 , caption = "Table 6: The resulting dataframe of the interaction-strength loss version of SimulateExtinctions")

## -----------------------------------------------------------------------------
data(dist)
dist

## -----------------------------------------------------------------------------
dist[,1:4] <- 0 # producers don't worry about rewiring
dist[5:10,5:8] <- 0 # intermediate consumders can only rewire to producers
dist[c(1:4, 9:10), 9:10] <- 0 # apex predators can only rewire to intermediate consumers
dist

## -----------------------------------------------------------------------------
RewiringDist <- function(x){x}

## ---- results='hide', fig.keep='all', message = FALSE-------------------------
Rewiring <- SimulateExtinctions(Network = net, Order = 1:2, Method = "Ordered", IS = 0.7,
                              Rewiring = function(x){x}, RewiringDist = dist, RewiringProb = 0.5)[[1]]

## ---- echo = FALSE------------------------------------------------------------
knitr::kable(Rewiring , caption = "Table 7: The resulting dataframe of the rewiring version of SimulateExtinctions")

