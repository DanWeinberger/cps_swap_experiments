key1 <-  read.table(file = './Data/Strain.clone.info.tsv', sep = '\t', header = TRUE) %>% rename(parent_ST=ID, new_ST=Strain , strain=Hartwell)

#are any genes added vs reference? 

plot.func.addition <- function(ds){
ds2 <- ds %>%
  mutate(strain= row.names(ds)) %>%
  left_join(key1, by='strain')%>%
  select(-strain, -parent_ST) %>%
  mutate(ID2 = paste(new_ST,Clone, sep='_'))

ds2a <- ds2 %>%
  mutate(ID2 = paste(new_ST,Clone, sep='_'))
row.names(ds2a) <- ds2a$ID2

mat1 <- ds2a %>%
  dplyr::select(-new_ST,-Clone, -ID2) %>%
  as.matrix()

groupvar=grep('group',colnames(mat1))

mat1 <- mat1[,-groupvar]

ref <- mat1[is.na(ds2a$new_ST) ,]

#is it discordant from reference?
#if they are equal, it returns 0, if present in x and absent in ref 1, if lost between ref and x =-1
disc <- t(apply(mat1, 1, function(x)  x-ref))
  
p <- heatmaply(disc, 
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
               labCol = colnames(disc),
               labRow = rownames(disc),
               heatmap_layers = theme(axis.line=element_blank())
)
return(p)
}
