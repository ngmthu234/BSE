# Load necessary libraries
library(tidvyverse)     # to manipute and graph data

# Create vector for PrP sequence
prp_sequence <-"MVKSHIGSWILVLFVAMWSDVGLCKKRPKPGGGWNTGGSRYPGQGSPGGNRYPPQGGGGWGQPHGGGWGQPHGGGWGQPHGGGWGQPHGGGWGQPHGGGGWGQGGTHGQWNKPSKPKTNMKHVAGAAAAGAVVGGLGGYMLGSAMSRPLIHFGSDYEDRYYRENMHRYPNQVYYRPVDQYSNQNNFVHDCVNITVKEHTVTTTTKGENFTETDIKMMERVVEQMCITQYQRESQAYYQRGASVILFSSPPVILLISFLIFLIVG"
prp_length <- nchar(prp_sequence)

# Create dataframe for domains of PrP
domain_data <- data.frame(domain = c("signal peptide", "octarepeat region", "\u03B1", "\u03B1", "\u03B1", "\u03B2", "\u03B2", "GPI"), start = c(1, 51, 144, 173, 200, 128, 161, 231), end = c(36, 103, 154, 194, 226, 131, 164, 264))

# Create dataframe for mutation of PrP
mutation_data <- data.frame(mutation = "E211K", mutation_type = "pathogenic")

# Create protein domain scheme
ggplot() 

     # to add PrP backbone
     + annotate("segment", x = 1, xend = prp_length, y = 0, yend = 0, color = "gray80", linewidth = 44.5) 
  
     # to add PrP protein domains
     + geom_rect(data = domain_data, aes(xmin = start, xmax = end, ymin = -0.01, ymax = 0.01, fill = domain)) 
     + geom_text(data = domain_data, aes(x = (start + end) / 2, y = 0, label = str_wrap(domain, width = 17)), size = 8, color = "white") 
     + scale_fill_manual(values = c("signal peptide" = "gray60", "octarepeat region" = "#a6cee3", "\u03B1" = "#6cacd9", "\u03B2" = "#288022", "GPI" = "gray40"), name = "PrP domains") 

     # to add E211K mutation
     + geom_segment(data = mutation_data, aes(x = 211, xend = 211, y = 0.01, yend = 0.03, color = mutation_type), linewidth = 3) 
     + geom_point(data = mutation_data, aes(x = 211, y = 0.03, color = mutation_type), size = 6, shape = 21, fill = "red", stroke = 2) 
     + geom_text(data = mutation_data, aes(x = 211, y = 0.035, label = mutation, color = mutation_type), color = "red", size = 8, fontface = "bold") 
     + scale_color_manual(values = c("pathogenic" = "red"), name = "Mutation") 
     
     # to add disulfide bond
     + annotate("segment", x = 179, xend = 214, y = -0.012, yend = -0.012, color = "black", linewidth = 1) 
     + annotate("segment", x = c(179, 214), xend = c(179, 214), y = -0.012, yend = -0.01, color = "black", linewidth = 1) 
     + annotate("text", label = "S", x = c(179, 214), y = -0.015, color = "black", size = 8)
     + annotate("text", label = c("179", "214"), x = c(179, 214), y = -0.019, color = "black", size = 3.5)

     # to add residue label to each protein domain start/end
     + annotate("text", label = c("1", "24", "51", "103", "144", "154", "173", "194", "200", "226", "128", "131", "161", "164", "231", "264"), x = c(1, 24, 51, 103, 144, 154, 173, 194, 200, 226, 128, 131, 161, 164, 231, 264), y  = 0.0115, color = "black", size = 3.5)

     # to add residue label for mature PrP 
     + annotate("text", label = c("23", "230"), x = c(23, 230), y - -0.015, color = "#fc9c0a", size = 8, fontfact = "bold")

     # to add N and C terminals
     + annotate("segment", x = -4, xend = 1, y = 0, color = "black", linewidth = 3) 
     + annotate("text", label = "N", x = -6, y = 0, color = "black", size = 8, fontface = "bold") 
     + annotate("segment", x = 264, xend = 268, y = 0, color = "black", linewidth = 3) 
     + annotate("text", label = "C", x = 270, y = 0, color = "black", size = 8, fontface = "bold") 
     
     # to customize the axes, theme, and grids
     + scale_x_continuous(name = NULL, breaks = seq(0, prp_length, by = 24), expand = expansion(add = c(5, 5))) 
     + scale_y_continuous(name = NULL, breaks = NULL, limits = c(-0.03, 0.2), expand = c(0, 0)) 
     + theme_minimal() 
     + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), axis.text.x = element_text(margin = margin(t = 5), size = 15)) 
