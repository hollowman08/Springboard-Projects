##Part 1 Creating rectangles based on distribution
# The initial contingency table
DF <- as.data.frame.matrix(table(adult$SRAGE_P, adult$RBMI))

# Add the columns groupsSum, xmax and xmin. Remove groupSum again.
DF$groupSum <- rowSums(DF)
DF$xmax <- cumsum(DF$groupSum)
DF$xmin <- DF$xmax - DF$groupSum
# The groupSum column needs to be removed, don't remove this line
DF$groupSum <- NULL

# Copy row names to variable X
DF$X <- row.names(DF)

# Melt the dataset
library(reshape2)
DF_melted <- melt(DF, id.vars = c("X", "xmin", "xmax"), variable.name = "FILL")

# dplyr call to calculate ymin and ymax - don't change
library(dplyr)
DF_melted <- DF_melted %>% 
     group_by(X) %>% 
     mutate(ymax = cumsum(value/sum(value)),
            ymin = ymax - value/sum(value))

# Plot rectangles - don't change.
library(ggthemes)
ggplot(DF_melted, aes(ymin = ymin, 
                      ymax = ymax,
                      xmin = xmin, 
                      xmax = xmax, 
                      fill = FILL)) + 
     geom_rect(colour = "white") +
     scale_x_continuous(expand = c(0,0)) +
     scale_y_continuous(expand = c(0,0)) +
     BMI_fill +
     theme_tufte()

##Part 2 Creating Mosaic Plot
# Perform chi.sq test (RBMI and SRAGE_P)
results <- chisq.test(table(adult$RBMI, adult$SRAGE_P))

# Melt results$residuals and store as resid
resid <- melt(results$residuals)

# Change names of resid
names(resid) <- c("FILL", "X", "residual")

# merge the two datasets:
DF_all <- merge(DF_melted, resid)

# Update plot command
library(ggthemes)
ggplot(DF_all, aes(ymin = ymin, 
                   ymax = ymax,
                   xmin = xmin, 
                   xmax = xmax, 
                   fill = residual)) + 
     geom_rect() +
     scale_fill_gradient2() +
     scale_x_continuous(expand = c(0,0)) +
     scale_y_continuous(expand = c(0,0)) +
     theme_tufte()

##Part 3 Adding text labels
# Position for labels on x axis
DF_all$xtext <- DF_all$xmin + (DF_all$xmax - DF_all$xmin)/2

# Position for labels on y axis (don't change)
index <- DF_all$xmax == max(DF_all$xmax)
DF_all$ytext <- DF_all$ymin[index] + (DF_all$ymax[index] - DF_all$ymin[index])/2

# Plot
ggplot(DF_all, aes(ymin = ymin, ymax = ymax, xmin = xmin, 
                   xmax = xmax, fill = residual)) + 
     geom_rect(col = "white") +
     # geom_text for ages (i.e. the x axis)
     geom_text(aes(x = xtext, 
                   label = X),
               y = 1,
               size = 3,
               angle = 90,
               hjust = 1,
               show.legend = FALSE) +
     # geom_text for BMI (i.e. the fill axis)
     geom_text(aes(x = max(xmax), 
                   y = ytext,
                   label = FILL),
               size = 3,
               hjust = 1,
               show.legend  = FALSE) +
     scale_fill_gradient2() +
     theme_tufte() +
     theme(legend.position = "bottom")