key1 <-  read.table(file = './Data/Strain.clone.info.tsv', sep = '\t', header = TRUE) %>% rename(parent_ST=ID, new_ST=Strain , strain=Hartwell)

plot.func <- function(ds){
ds2 <- ds %>%
  mutate(strain= row.names(ds)) %>%
  left_join(key1, by='strain')%>%
  select(-strain, -parent_ST) %>%
  mutate(ID2 = paste(new_ST,Clone, sep='_'))

test <- unique(ds)
nrow(test)==nrow(ds) #if T, no rows removed

ds2a <- ds2 %>%
  mutate(ID2 = paste(new_ST,Clone, sep='_'))
row.names(ds2a) <- ds2a$ID2

mat1 <- ds2a %>%
  dplyr::select(-new_ST,-Clone, -ID2) %>%
  as.matrix()

groupvar=grep('group',colnames(mat1))

mat1 <- mat1[,-groupvar]

p <- heatmaply(mat1, 
               #dendrogram = "row",
               xlab = "", ylab = "", 
               main = "",
               scale = "none",
               margins = c(60,100,40,20),
               grid_gap=0,
               titleX = FALSE,
               hide_colorbar = TRUE,
               branches_lwd = 0.1,
               label_names = c("Serotype/isolate", "Feature:", "Value"),
               fontsize_row = 5, fontsize_col = 5,
               labCol = colnames(mat1),
               labRow = rownames(mat1),
               heatmap_layers = theme(axis.line=element_blank())
)
return(p)
}
