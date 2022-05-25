library(dplyr)
#bind new table with old tables

#2018 tables

  t1 <- readxl::read_excel("18-FDA-01 Carbapenemase Database_To sponsor.xlsx")
  
  t2 <- readxl::read_excel("18-FDA-01 Fluconazole Resistance Database_To sponsor_GD_10-28-2019.xlsx", sheet = "Single Gene Mutations")

  t3 <- readxl::read_excel("18-FDA-01 MRSA Resistance Database_To sponsor.xlsx", sheet = "18-FDA-01 MRSA Resistance Datab")

  
  
  
#notes
  #for t2 take amino acid subs 1 through 11 and paste them together in a Mutation column
    t2$Mutation <- ""
    for(i in 1:nrow(t2)){
      t2$Mutation[i] <- paste(if(is.na(t2$`Gene One Amino Acid Substitution 1`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 1`[i]}, 
                              if(is.na(t2$`Gene One Amino Acid Substitution 2`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 2`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 3`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 3`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 4`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 4`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 5`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 5`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 6`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 6`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 7`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 7`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 8`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 8`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 9`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 9`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 10`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 10`[i]},
                              if(is.na(t2$`Gene One Amino Acid Substitution 11`[i] == TRUE)){""}else{t2$`Gene One Amino Acid Substitution 11`[i]}
                              , sep = ",")
    }
  
  
#2020 tables
  t4 <- readxl::read_excel("20-FDA-01_4may2022.xlsx")

  
  
  
#add the study year to all the tables
  t1$Study_Year <- 2018
  t2$Study_Year <- 2018
  t3$Study_Year <- 2018
  t4$Study_Year <- 2021
  
  
#add unique identifiers to tables
  t1$row_num <- seq.int(nrow(t1))
  t2$row_num <- seq.int(nrow(t2))
  t3$row_num <- seq.int(nrow(t3))
  t4$row_num <- seq.int(nrow(t4))
  
  t1$original_table <- "t1"
  t2$original_table <- "t2"
  t3$original_table <- "t3"
  t4$original_table <- "t4"
  
  
#ADD columns that need added
  t3$Species <- "Staph aureus"
  t2$Drug <- "Fluconazole"
  t3$Drug <- "Oxacillin/Methicillin"
  
  #Columns all 4 have:  Study_Year, Gene Abbreviation, Clinical Species, 
  #                       Level of evidence, drug, full gene name
  t1_a <- t1 %>%
            select(original_table, row_num, Study_Year, `Gene Abbreviation`, 
                   `Clinical Species`, `Level of Evidence`, Drug, `Full Gene Name`)
  colnames(t1_a) <- c("original_table", "row_num", "Study_Year", "Gene_Abbreviation", "Clinical_Species", 
                      "Level_of_Evidence", "Drug_for_evidence", "Full_Gene_Name")

  t2_a <- t2 %>%
    select(original_table, row_num, Study_Year, `Gene Abbreviation`, 
           Species, `Level of Evidence`, Drug, `Full Gene Name`)
  colnames(t2_a) <- c("original_table", "row_num", "Study_Year", "Gene_Abbreviation", "Clinical_Species", 
                      "Level_of_Evidence", "Drug_for_evidence", "Full_Gene_Name")
  
  t3_a <- t3 %>%
    select(original_table, row_num, Study_Year, `Gene Abbreviation`, 
           Species, `Level of Evidence`, Drug, `Full Gene Name`)
  colnames(t3_a) <- c("original_table", "row_num", "Study_Year", "Gene_Abbreviation", "Clinical_Species", 
                      "Level_of_Evidence", "Drug_for_evidence", "Full_Gene_Name")
  
  t4_a <- t4 %>%
    select(original_table, row_num, Study_Year, `Gene Name`, 
           `Species of clinical evidence`, `Level of Evidence`, `Drug for evidence`, `Full Gene Name`)
  colnames(t4_a) <- c("original_table", "row_num", "Study_Year", "Gene_Abbreviation", "Clinical_Species", 
                      "Level_of_Evidence", "Drug_for_evidence", "Full_Gene_Name")

  
#start final table
  ft <- rbind(t1_a, t2_a, t3_a, t4_a)
  
  

  
#columns with 3 tables that include the column
  #add columns that need to be added
  t1$Gene_Function <- NA
  t2$NCBI_Accession_Number <- NA
  t2$Amino_Acid_Sequence <- NA
  t2$Accession_Number_Has_Linked_Publication <- NA
  t2$MIC_Value_for_Clinical_Isolate <- "Fluconazole MIC Value"
  t3$MIC_Value_for_Clinical_Isolate <- NA
  t4$Sources <- NA
  t2$Year_of_Clinical_Publication <- NA
  t3$Summary_of_Evidence <- NA
  t3$Comments <- NA
  
  
  t1_b <- t1 %>%
            select(original_table, row_num, `NCBI Accession Number`, `Amino Acid Sequence`, 
                   `Accession Number Has Linked Publication`, `MIC Value for Clinical Isolate`, 
                   `Clinical Evidence Source (URL)`, `Year of Clinical Publication`, 
                   `Summary of Evidence`, Comments, "Gene_Function")
  colnames(t1_b) <- c("original_table", "row_num", "NCBI_Accession_Number", "Amino_Acid_Sequence", 
                      "Accession_Number_Has_Linked_Publication", "MIC_Value_for_Clinical_Isolate", 
                      "Clinical_Evidence_Source_URL", "Year_of_Clinical_Publication", 
                      "Summary_of_Evidence", "Comments", "Gene_Function")
  
  
  t2_b <- t2 %>%
    select(original_table, row_num, NCBI_Accession_Number, Amino_Acid_Sequence, 
           Accession_Number_Has_Linked_Publication, MIC_Value_for_Clinical_Isolate, 
           Sources, Year_of_Clinical_Publication, 
           `Causative evidence`, `Notes:`, `Gene Function`)
  colnames(t2_b) <- c("original_table", "row_num", "NCBI_Accession_Number", "Amino_Acid_Sequence", 
                      "Accession_Number_Has_Linked_Publication", "MIC_Value_for_Clinical_Isolate", 
                      "Clinical_Evidence_Source_URL", "Year_of_Clinical_Publication", 
                      "Summary_of_Evidence", "Comments", "Gene_Function")
  
  t3_b <- t3 %>%
    select(original_table, row_num, `NCBI Accession Number`, Sequence, 
           `Has Linked Publication`, MIC_Value_for_Clinical_Isolate, 
           Sources, `Publication Year`, 
           Summary_of_Evidence, Comments, `Gene Function`)
  colnames(t3_b) <- c("original_table", "row_num", "NCBI_Accession_Number", "Amino_Acid_Sequence", 
                      "Accession_Number_Has_Linked_Publication", "MIC_Value_for_Clinical_Isolate", 
                      "Clinical_Evidence_Source_URL", "Year_of_Clinical_Publication", 
                      "Summary_of_Evidence", "Comments", "Gene_Function")

  t4_b <- t4 %>%
    select(original_table, row_num, `NCBI Accession Number - Nucleotid`, `Amino Acid Sequence`, 
           `NCBI Accession Number Has Linked Publication`, `MIC of evidence isolate`, 
           Sources, `Evidence publication year`, 
           `Summary of Evidence`, `Comments/notes`, `Gene Function`)
  colnames(t4_b) <- c("original_table", "row_num", "NCBI_Accession_Number", "Amino_Acid_Sequence", 
                      "Accession_Number_Has_Linked_Publication", "MIC_Value_for_Clinical_Isolate", 
                      "Clinical_Evidence_Source_URL", "Year_of_Clinical_Publication", 
                      "Summary_of_Evidence", "Comments", "Gene_Function")  

  ft_2 <- rbind(t1_b, t2_b, t3_b, t4_b)  
  
  
  
  
  
  
  
  
#Do all the variables that are present in just 2 of the datasets
  t1$Method_for_MIC_Values <- NA
  t1$Mutation<- NA
  t1$Type_of_Genetic_Marker<- NA
  t1$Additional_Information<- NA
  t1$SCCmec_Element<- NA
  t1$Aliases<- NA
  t1$Resistant_Mechanism<- NA
  t1$Gene_promotor_mutation <- NA
  t1$Gene_Promotor_NT_Substitution <- NA
  
  t1_c <- t1 %>%
            select(original_table, row_num,`NCBI Accession Number Species`, 
                   `NCBI Submitted Sequence - Isolate Source`, `NCBI Submitted Sequence - Isolate ID Method`, `Geographic Location of Submitted Sequence Isolate`,
                   `Clinical Evidence in >10 Isolates`, `Clinical Evidence Is Affiliated with Accession #`, `Breakpoint Organization`,
                   `Susceptibility Method for Clinical Isolate(s)`, `Transformation Experiment Performed`, `Transformation Is Affiliated with Accession #`,
                   `Transformation Experiment Source (URL)`, `Transformant Species`, `MIC Value for Transformant`,
                   `Fold Change in Transformant`, `MIC Interpretation for Transformant`, `Hydrolysis Is Associated with Accession #`,
                   `Hydrolysis Results`, "Method_for_MIC_Values", "Mutation", 
                   "Type_of_Genetic_Marker", "Additional_Information", "SCCmec_Element",
                   "Aliases", "Resistant_Mechanism", Gene_promotor_mutation, Gene_Promotor_NT_Substitution
                   )
  colnames(t1_c) <- c("original_table", "row_num","NCBI_Accession_Number_Species", 
                      "NCBI_Submitted_Sequence_Isolate_Source", "NCBI_Submitted_Sequence_Isolate_ID_Method", "Geographic_Location_of_Submitted_Sequence_Isolate",
                      "Clinical_Evidence_in_over_10_Isolates", "Clinical_Evidence_Is_Affiliated_with_Accession_Number", "Breakpoint_Organization",
                      "Susceptibility_Method_for_Clinical_Isolates", "Transformation_Experiment_Performed", "Transformation_Is_Affiliated_with_Accession_Number",
                      "Transformation_Experiment_Source_URL", "Transformant_Species", "MIC_Value_for_Transformant",
                      "Fold_Change_in_Transformant", "MIC_Interpretation_for_Tranformant", "Hydrolysis_Is_Associated_with_Accession_Number",
                      "Hydrolysis_Results", "Method_for_MIC_Values", "Mutation", 
                      "Type_of_Genetic_Marker", "Additional_Information", "SCCmec_Element",
                      "Aliases", "Resistant_Mechanism", "Gene_promotor_mutation", "Gene_Promotor_NT_Substitution")
  
  
  t2$Transformation_Experiment_Source_URL <- NA
  t2$Transformant_Species<- NA
  t2$MIC_Value_for_Transformant<- NA
  t2$Fold_Change_in_Transformant<- NA
  t2$MIC_Interpretation_for_Tranformant<- NA
  t2$Breakpoint_Organization<- NA
  t2$Geographic_Location_of_Submitted_Sequence_Isolate<- NA
  t2$Transformation_Experiment_Performed<- NA
  t2$Susceptibility_Method_for_Clinical_Isolates<- NA
  t2$Clinical_Evidence_Is_Affiliated_with_Accession_Number<- NA
  t2$Clinical_Evidence_in_over_10_Isolates<- NA
  t2$NCBI_Submitted_Sequence_Isolate_ID_Method<- NA
  t2$NCBI_Submitted_Sequence_Isolate_Source<- NA
  t2$NCBI_Accession_Number_Species<- NA
  t2$Hydrolysis_Is_Associated_with_Accession_Number<- NA
  t2$Hydrolysis_Results<- NA
  t2$Transformation_Is_Affiliated_with_Accession_Number <- NA
  t2$SCCmec_Element <- NA
  t2$Aliases <- NA
  t2$Resistant_Mechanism <- NA
    
  
  t2_c <- t2 %>%
    select(original_table, row_num, NCBI_Accession_Number_Species, 
           NCBI_Submitted_Sequence_Isolate_Source, NCBI_Submitted_Sequence_Isolate_ID_Method, Geographic_Location_of_Submitted_Sequence_Isolate,
           Clinical_Evidence_in_over_10_Isolates, Clinical_Evidence_Is_Affiliated_with_Accession_Number, Breakpoint_Organization,
           Susceptibility_Method_for_Clinical_Isolates, Transformation_Experiment_Performed, Transformation_Is_Affiliated_with_Accession_Number,
           Transformation_Experiment_Source_URL, Transformant_Species, MIC_Value_for_Transformant,
           Fold_Change_in_Transformant, MIC_Interpretation_for_Tranformant, Hydrolysis_Is_Associated_with_Accession_Number,
           Hydrolysis_Results, `Method for MIC values`, Mutation, 
           `Type of Genetic Marker`, `Additional information`, SCCmec_Element,
           Aliases, Resistant_Mechanism, `Gene promoter mutation`, `Gene Promoter NT substitution`
    )
  colnames(t2_c) <- c("original_table", "row_num","NCBI_Accession_Number_Species", 
                         "NCBI_Submitted_Sequence_Isolate_Source", "NCBI_Submitted_Sequence_Isolate_ID_Method", "Geographic_Location_of_Submitted_Sequence_Isolate",
                         "Clinical_Evidence_in_over_10_Isolates", "Clinical_Evidence_Is_Affiliated_with_Accession_Number", "Breakpoint_Organization",
                         "Susceptibility_Method_for_Clinical_Isolates", "Transformation_Experiment_Performed", "Transformation_Is_Affiliated_with_Accession_Number",
                         "Transformation_Experiment_Source_URL", "Transformant_Species", "MIC_Value_for_Transformant",
                         "Fold_Change_in_Transformant", "MIC_Interpretation_for_Tranformant", "Hydrolysis_Is_Associated_with_Accession_Number",
                         "Hydrolysis_Results", "Method_for_MIC_Values", "Mutation", 
                         "Type_of_Genetic_Marker", "Additional_Information", "SCCmec_Element",
                         "Aliases", "Resistant_Mechanism", "Gene_promotor_mutation", "Gene_Promotor_NT_Substitution")
  
  
  
  
  
  
  t3$NCBI_Submitted_Sequence_Isolate_Source<- NA
  t3$NCBI_Submitted_Sequence_Isolate_ID_Method<- NA
  t3$Transformation_Experiment_Source_URL <- NA
  t3$Transformant_Species<- NA
  t3$MIC_Value_for_Transformant<- NA
  t3$Fold_Change_in_Transformant<- NA
  t3$MIC_Interpretation_for_Tranformant<- NA
  t3$Breakpoint_Organization<- NA
  t3$Geographic_Location_of_Submitted_Sequence_Isolate<- NA
  t3$Transformation_Experiment_Performed<- NA
  t3$Susceptibility_Method_for_Clinical_Isolates<- NA
  t3$Clinical_Evidence_Is_Affiliated_with_Accession_Number<- NA
  t3$Clinical_Evidence_in_over_10_Isolates<- NA
  t3$Hydrolysis_Is_Associated_with_Accession_Number<- NA
  t3$Hydrolysis_Results<- NA
  t3$Transformation_Is_Affiliated_with_Accession_Number <- NA
  t3$Method_for_MIC_Values <- NA
  t3$Additional_Information <- NA
  t3$Mutation <- NA
  t3$Gene_promotor_mutation <- NA
  t3$Gene_Promotor_NT_Substitution <- NA
  
  t3_c <- t3 %>%
    select(original_table, row_num, `NCBI Accession Number Species`, 
           NCBI_Submitted_Sequence_Isolate_Source, NCBI_Submitted_Sequence_Isolate_ID_Method, Geographic_Location_of_Submitted_Sequence_Isolate,
           Clinical_Evidence_in_over_10_Isolates, Clinical_Evidence_Is_Affiliated_with_Accession_Number, Breakpoint_Organization,
           Susceptibility_Method_for_Clinical_Isolates, Transformation_Experiment_Performed, Transformation_Is_Affiliated_with_Accession_Number,
           Transformation_Experiment_Source_URL, Transformant_Species, MIC_Value_for_Transformant,
           Fold_Change_in_Transformant, MIC_Interpretation_for_Tranformant, Hydrolysis_Is_Associated_with_Accession_Number,
           Hydrolysis_Results, Method_for_MIC_Values, Mutation, 
           `Type of Genetic Marker`, Additional_Information, `SCCmec element`,
           Aliases, `Mechanism of Resistance`, Gene_promotor_mutation, Gene_Promotor_NT_Substitution
    )
  colnames(t3_c) <- c("original_table", "row_num","NCBI_Accession_Number_Species", 
                      "NCBI_Submitted_Sequence_Isolate_Source", "NCBI_Submitted_Sequence_Isolate_ID_Method", "Geographic_Location_of_Submitted_Sequence_Isolate",
                      "Clinical_Evidence_in_over_10_Isolates", "Clinical_Evidence_Is_Affiliated_with_Accession_Number", "Breakpoint_Organization",
                      "Susceptibility_Method_for_Clinical_Isolates", "Transformation_Experiment_Performed", "Transformation_Is_Affiliated_with_Accession_Number",
                      "Transformation_Experiment_Source_URL", "Transformant_Species", "MIC_Value_for_Transformant",
                      "Fold_Change_in_Transformant", "MIC_Interpretation_for_Tranformant", "Hydrolysis_Is_Associated_with_Accession_Number",
                      "Hydrolysis_Results", "Method_for_MIC_Values", "Mutation", 
                      "Type_of_Genetic_Marker", "Additional_Information", "SCCmec_Element",
                      "Aliases", "Resistant_Mechanism", "Gene_promotor_mutation", "Gene_Promotor_NT_Substitution")
  
  
  t4$NCBI_Accession_Number_Species <- NA
  t4$Type_of_Genetic_Marker <- NA
  
  t4_c <- t4 %>%
            select(original_table, row_num, NCBI_Accession_Number_Species,
                   `NCBI Submitted Sequence - Isolate Source`, `NCBI Submitted Sequence - Isolate ID Method`, `Geographic Location of evidence associated isolate`,
                   `Clinical Evidence in >10 Isolates`, `Clinical Evidence Is Affiliated with Accession #`, `Breakpoint Organization for interpretation for experimental confirmation`,
                   `Susceptibility testing method for experimental confirmation isolate`, `Method to generate experimental confirmation`, `Accession number for experimental confirmation`,
                   `URL for level of evidence determination`, `Species of experimental confirmation isolate`, `MIC of experimental confirmation isolate`,
                   `MIC fold change for experimental confirmation`, `Interpretation for MIC of evidence associated isolate`, `Hydrolysis Is Associated with Accession #`,
                   `Hydrolysis Results`, `Susceptibility testing method for evidence associated isolate`, Mutation,
                   Type_of_Genetic_Marker, `Additional information`, `SCCmec element`, 
                   `Gene name aliases`,`Resistant Mechanism`, `Gene promoter mutation`, `Gene Promoter NT substitution`
                   )
  colnames(t4_c) <- c("original_table", "row_num","NCBI_Accession_Number_Species", 
                      "NCBI_Submitted_Sequence_Isolate_Source", "NCBI_Submitted_Sequence_Isolate_ID_Method", "Geographic_Location_of_Submitted_Sequence_Isolate",
                      "Clinical_Evidence_in_over_10_Isolates", "Clinical_Evidence_Is_Affiliated_with_Accession_Number", "Breakpoint_Organization",
                      "Susceptibility_Method_for_Clinical_Isolates", "Transformation_Experiment_Performed", "Transformation_Is_Affiliated_with_Accession_Number",
                      "Transformation_Experiment_Source_URL", "Transformant_Species", "MIC_Value_for_Transformant",
                      "Fold_Change_in_Transformant", "MIC_Interpretation_for_Tranformant", "Hydrolysis_Is_Associated_with_Accession_Number",
                      "Hydrolysis_Results", "Method_for_MIC_Values", "Mutation", 
                      "Type_of_Genetic_Marker", "Additional_Information", "SCCmec_Element",
                      "Aliases", "Resistant_Mechanism", "Gene_promotor_mutation", "Gene_Promotor_NT_Substitution")
  
  
  ft_3 <- rbind(t1_c, t2_c, t3_c, t4_c)
  
  
  
  
  
  
  
  
  #do all the variables that are just in 1 table

  t1$Alteration_needed_for_resistance <- NA
  t1$Strain <- NA
  t1$Clinical_or_lab_generated <- NA
  t1$Number_of_mutations_in_gene <- NA
  t1$Single_mutation_known_to_cause_resistance <- NA
  t1$Causitive_evidence_source <- NA
  t1$Deletion_Transformation_Point_mutation <- NA
  t1$Type_of_lab_generated_strain <- NA
  t1$Method_to_generate_mutant <- NA
  t1$Fungal_species_using_in_lab_experiment <- NA
  t1$MIC_method_for_causal_evidence <- NA
  t1$Fluconazole_transformant_or_knockout_MIC_value <- NA
  t1$Fold_change <- NA
  t1$Target_of_Transcription_Factor <- NA
  t1$Downstream_Effect <- NA
  
  t1$Resistance_phenotype <- NA
  
  t1$Nucleotide_Sequence <- NA
  t1$NCBI_Accession_Number_Protein <- NA
  t1$NCBI_Accession_Number_PubMed_link <- NA
  t1$Breakpoint_applied_for_MIC_of_evidence_associated_isolate <- NA
  t1$Additional_mutations_required_for_resistance <- NA
  t1$Gene_has_multiple_GenBank_entries <- NA
  
  t1_d <- t1 %>%
            select(original_table, row_num,
                   
                   `Specific Cephalosporin`, `Organism Group`, `Submission Type`,
                   `Gene Has Multiple Species Entries?`, `MIC Interpretation for Clinical Isolate`, `Susceptibility Method for Transformant`,
                   `Hydrolysis Experiment Source (URL)`,
                   
                   "Alteration_needed_for_resistance", "Strain", "Clinical_or_lab_generated",
                   "Number_of_mutations_in_gene", "Single_mutation_known_to_cause_resistance",
                   "Causitive_evidence_source", "Deletion_Transformation_Point_mutation", "Type_of_lab_generated_strain",
                   "Method_to_generate_mutant", "Fungal_species_using_in_lab_experiment", "MIC_method_for_causal_evidence",
                   "Fluconazole_transformant_or_knockout_MIC_value", "Fold_change", "Target_of_Transcription_Factor",
                   "Downstream_Effect",
                   
                   "Resistance_phenotype",
                   
                   "Nucleotide_Sequence", "NCBI_Accession_Number_Protein", "NCBI_Accession_Number_PubMed_link",
                   "Breakpoint_applied_for_MIC_of_evidence_associated_isolate", "Additional_mutations_required_for_resistance",
                   "Gene_has_multiple_GenBank_entries"
                   
                   )
  colnames(t1_d) <- c("original_table", "row_num",
                      
                      "Specific_Cephalosporin", "Organism_Group", "Submission_Type",
                      "Gene_Has_Multiple_Species_Entries", "MIC_Interpretation_for_Clinical_Isolate", "Susceptibility_Method_for_Transformant",
                      "Hydrolysis_Experiment_Source_URL",
                      
                      "Alteration_needed_for_resistance", "Strain", "Clinical_or_lab_generated",
                      "Number_of_mutations_in_gene", "Single_mutation_known_to_cause_resistance",
                      "Causitive_evidence_source", "Deletion_Transformation_Point_mutation", "Type_of_lab_generated_strain",
                      "Method_to_generate_mutant", "Fungal_species_using_in_lab_experiment", "MIC_method_for_causal_evidence",
                      "Fluconazole_transformant_or_knockout_MIC_value", "Fold_change", "Target_of_Transcription_Factor",
                      "Downstream_Effect",
                      
                      "Resistance_phenotype",
                      
                      "Nucleotide_Sequence", "NCBI_Accession_Number_Protein", "NCBI_Accession_Number_PubMed_link",
                      "Breakpoint_applied_for_MIC_of_evidence_associated_isolate", "Additional_mutations_required_for_resistance",
                      "Gene_has_multiple_GenBank_entries"
                      
  )
  
  
  t2$Specific_Cephalosporin <- NA
  t2$Organism_Group <- NA
  t2$Submission_Type <- NA
  t2$Gene_Has_Multiple_Species_Entries <- NA
  t2$MIC_Interpretation_for_Clinical_Isolate <- NA
  t2$Susceptibility_Method_for_Transformant <- NA
  t2$Hydrolysis_Experiment_Source_URL  <- NA
  
  t2$Resistance_phenotype <- NA
  
  t2$Nucleotide_Sequence <- NA
  t2$NCBI_Accession_Number_Protein <- NA
  t2$NCBI_Accession_Number_PubMed_link <- NA
  t2$Breakpoint_applied_for_MIC_of_evidence_associated_isolate <- NA
  t2$Additional_mutations_required_for_resistance <- NA
  t2$Gene_has_multiple_GenBank_entries <- NA
  
  t2_d <- t2 %>%
            select(original_table, row_num,
                   
                   "Specific_Cephalosporin", "Organism_Group", "Submission_Type",
                   "Gene_Has_Multiple_Species_Entries", "MIC_Interpretation_for_Clinical_Isolate", "Susceptibility_Method_for_Transformant",
                   "Hydrolysis_Experiment_Source_URL",
                   
                   `Alteration needed for resistance`, Strain, `Clinical or lab generated`,
                   `Number of mutations in gene`, `Single mutation known to cause resistance (can contain multiple)`,
                   `Causitive evidence source`, `Deletion/Transformation/Point mutation`, `Type of lab generated strain`,
                   `Method to generate mutant`, `Fungal species using in lab experiment`, `MIC method for causal evidence`,
                   `Fluconazole transformant/knockout MIC value`, `Fold change`, `Target of Transcription Factor`,
                   `Downstream Effect`,
                   "Resistance_phenotype",
                   
                   "Nucleotide_Sequence", "NCBI_Accession_Number_Protein", "NCBI_Accession_Number_PubMed_link",
                   "Breakpoint_applied_for_MIC_of_evidence_associated_isolate", "Additional_mutations_required_for_resistance",
                   "Gene_has_multiple_GenBank_entries"
                   )
  colnames(t2_d) <- c("original_table", "row_num",
                      
                      "Specific_Cephalosporin", "Organism_Group", "Submission_Type",
                      "Gene_Has_Multiple_Species_Entries", "MIC_Interpretation_for_Clinical_Isolate", "Susceptibility_Method_for_Transformant",
                      "Hydrolysis_Experiment_Source_URL",
                      
                      "Alteration_needed_for_resistance", "Strain", "Clinical_or_lab_generated",
                      "Number_of_mutations_in_gene", "Single_mutation_known_to_cause_resistance",
                      "Causitive_evidence_source", "Deletion_Transformation_Point_mutation", "Type_of_lab_generated_strain",
                      "Method_to_generate_mutant", "Fungal_species_using_in_lab_experiment", "MIC_method_for_causal_evidence",
                      "Fluconazole_transformant_or_knockout_MIC_value", "Fold_change", "Target_of_Transcription_Factor",
                      "Downstream_Effect",
                      
                      "Resistance_phenotype",
                      
                      "Nucleotide_Sequence", "NCBI_Accession_Number_Protein", "NCBI_Accession_Number_PubMed_link",
                      "Breakpoint_applied_for_MIC_of_evidence_associated_isolate", "Additional_mutations_required_for_resistance",
                      "Gene_has_multiple_GenBank_entries"
                      
  )
  
  t3$Specific_Cephalosporin <- NA
  t3$Organism_Group <- NA
  t3$Submission_Type <- NA
  t3$Gene_Has_Multiple_Species_Entries <- NA
  t3$MIC_Interpretation_for_Clinical_Isolate <- NA
  t3$Susceptibility_Method_for_Transformant <- NA
  t3$Hydrolysis_Experiment_Source_URL  <- NA
  
  t3$Alteration_needed_for_resistance <- NA
  t3$Strain <- NA
  t3$Clinical_or_lab_generated <- NA
  t3$Number_of_mutations_in_gene <- NA
  t3$Single_mutation_known_to_cause_resistance <- NA
  t3$Causitive_evidence_source <- NA
  t3$Deletion_Transformation_Point_mutation <- NA
  t3$Type_of_lab_generated_strain <- NA
  t3$Method_to_generate_mutant <- NA
  t3$Fungal_species_using_in_lab_experiment <- NA
  t3$MIC_method_for_causal_evidence <- NA
  t3$Fluconazole_transformant_or_knockout_MIC_value <- NA
  t3$Fold_change <- NA
  t3$Target_of_Transcription_Factor <- NA
  t3$Downstream_Effect <- NA
  
  t3$Nucleotide_Sequence <- NA
  t3$NCBI_Accession_Number_Protein <- NA
  t3$NCBI_Accession_Number_PubMed_link <- NA
  t3$Breakpoint_applied_for_MIC_of_evidence_associated_isolate <- NA
  t3$Additional_mutations_required_for_resistance <- NA
  t3$Gene_has_multiple_GenBank_entries <- NA
  
  t3_d <- t3 %>%
            select(original_table, row_num,
                   
                   "Specific_Cephalosporin", "Organism_Group", "Submission_Type",
                   "Gene_Has_Multiple_Species_Entries", "MIC_Interpretation_for_Clinical_Isolate", "Susceptibility_Method_for_Transformant",
                   "Hydrolysis_Experiment_Source_URL",
                   
                   "Alteration_needed_for_resistance", "Strain", "Clinical_or_lab_generated",
                   "Number_of_mutations_in_gene", "Single_mutation_known_to_cause_resistance",
                   "Causitive_evidence_source", "Deletion_Transformation_Point_mutation", "Type_of_lab_generated_strain",
                   "Method_to_generate_mutant", "Fungal_species_using_in_lab_experiment", "MIC_method_for_causal_evidence",
                   "Fluconazole_transformant_or_knockout_MIC_value", "Fold_change", "Target_of_Transcription_Factor",
                   "Downstream_Effect",
                   
                   `Resistance phenotype`,
                   
                   "Nucleotide_Sequence", "NCBI_Accession_Number_Protein", "NCBI_Accession_Number_PubMed_link",
                   "Breakpoint_applied_for_MIC_of_evidence_associated_isolate", "Additional_mutations_required_for_resistance",
                   "Gene_has_multiple_GenBank_entries"
                   
                   )
  colnames(t3_d) <- c("original_table", "row_num",
                      
                      "Specific_Cephalosporin", "Organism_Group", "Submission_Type",
                      "Gene_Has_Multiple_Species_Entries", "MIC_Interpretation_for_Clinical_Isolate", "Susceptibility_Method_for_Transformant",
                      "Hydrolysis_Experiment_Source_URL",
                      
                      "Alteration_needed_for_resistance", "Strain", "Clinical_or_lab_generated",
                      "Number_of_mutations_in_gene", "Single_mutation_known_to_cause_resistance",
                      "Causitive_evidence_source", "Deletion_Transformation_Point_mutation", "Type_of_lab_generated_strain",
                      "Method_to_generate_mutant", "Fungal_species_using_in_lab_experiment", "MIC_method_for_causal_evidence",
                      "Fluconazole_transformant_or_knockout_MIC_value", "Fold_change", "Target_of_Transcription_Factor",
                      "Downstream_Effect",
                      
                      "Resistance_phenotype",
                      
                      "Nucleotide_Sequence", "NCBI_Accession_Number_Protein", "NCBI_Accession_Number_PubMed_link",
                      "Breakpoint_applied_for_MIC_of_evidence_associated_isolate", "Additional_mutations_required_for_resistance",
                      "Gene_has_multiple_GenBank_entries"
                      
  )
  
  
  t4$Specific_Cephalosporin <- NA
  t4$Organism_Group <- NA
  t4$Submission_Type <- NA
  t4$Gene_Has_Multiple_Species_Entries <- NA
  t4$MIC_Interpretation_for_Clinical_Isolate <- NA
  t4$Susceptibility_Method_for_Transformant <- NA
  t4$Hydrolysis_Experiment_Source_URL  <- NA
  
  t4$Alteration_needed_for_resistance <- NA
  t4$Strain <- NA
  t4$Clinical_or_lab_generated <- NA
  t4$Number_of_mutations_in_gene <- NA
  t4$Single_mutation_known_to_cause_resistance <- NA
  t4$Causitive_evidence_source <- NA
  t4$Deletion_Transformation_Point_mutation <- NA
  t4$Type_of_lab_generated_strain <- NA
  t4$Method_to_generate_mutant <- NA
  t4$Fungal_species_using_in_lab_experiment <- NA
  t4$MIC_method_for_causal_evidence <- NA
  t4$Fluconazole_transformant_or_knockout_MIC_value <- NA
  t4$Fold_change <- NA
  t4$Target_of_Transcription_Factor <- NA
  t4$Downstream_Effect <- NA
  
  t4$Resistance_phenotype <- NA
  
  
  t4_d <- t4 %>%
            select("original_table", "row_num",
                   
                   "Specific_Cephalosporin", "Organism_Group", "Submission_Type",
                   "Gene_Has_Multiple_Species_Entries", "MIC_Interpretation_for_Clinical_Isolate", "Susceptibility_Method_for_Transformant",
                   "Hydrolysis_Experiment_Source_URL",
                   
                   "Alteration_needed_for_resistance", "Strain", "Clinical_or_lab_generated",
                   "Number_of_mutations_in_gene", "Single_mutation_known_to_cause_resistance",
                   "Causitive_evidence_source", "Deletion_Transformation_Point_mutation", "Type_of_lab_generated_strain",
                   "Method_to_generate_mutant", "Fungal_species_using_in_lab_experiment", "MIC_method_for_causal_evidence",
                   "Fluconazole_transformant_or_knockout_MIC_value", "Fold_change", "Target_of_Transcription_Factor",
                   "Downstream_Effect",
                   
                   "Resistance_phenotype",
                   
                   `Nucleotide Sequence`, `NCBI Accession Number - Protein`, `NCBI Accession Number- PubMed link`,
                   `Breakpoint applied for MIC of evidence associated isolate`, `Additional mutations required for resistance?`,
                   `Gene has multiple GenBank entries`
                   )
  colnames(t4_d) <- c("original_table", "row_num",
                      
                      "Specific_Cephalosporin", "Organism_Group", "Submission_Type",
                      "Gene_Has_Multiple_Species_Entries", "MIC_Interpretation_for_Clinical_Isolate", "Susceptibility_Method_for_Transformant",
                      "Hydrolysis_Experiment_Source_URL",
                      
                      "Alteration_needed_for_resistance", "Strain", "Clinical_or_lab_generated",
                      "Number_of_mutations_in_gene", "Single_mutation_known_to_cause_resistance",
                      "Causitive_evidence_source", "Deletion_Transformation_Point_mutation", "Type_of_lab_generated_strain",
                      "Method_to_generate_mutant", "Fungal_species_using_in_lab_experiment", "MIC_method_for_causal_evidence",
                      "Fluconazole_transformant_or_knockout_MIC_value", "Fold_change", "Target_of_Transcription_Factor",
                      "Downstream_Effect",
                      
                      "Resistance_phenotype",
                      
                       "Nucleotide_Sequence", "NCBI_Accession_Number_Protein", "NCBI_Accession_Number_PubMed_link",
                       "Breakpoint_applied_for_MIC_of_evidence_associated_isolate", "Additional_mutations_required_for_resistance",
                       "Gene_has_multiple_GenBank_entries"
                      
  )
  
  
  
  ft_4 <- rbind(t1_d, t2_d, t3_d, t4_d)
  
  
#join all four tables together
  final_table <- sqldf::sqldf("select a.*, b.NCBI_Accession_Number, b.Amino_Acid_Sequence, b.Accession_Number_Has_Linked_Publication,
                                        b.MIC_Value_for_Clinical_Isolate, b.Clinical_Evidence_Source_URL, b.Year_of_Clinical_Publication,
                                        b.Summary_of_Evidence, b.Comments, b.Gene_Function,
                                        c.NCBI_Accession_Number_Species, 
                                        c.NCBI_Submitted_Sequence_Isolate_Source, c.NCBI_Submitted_Sequence_Isolate_ID_Method, c.Geographic_Location_of_Submitted_Sequence_Isolate,
                                        c.Clinical_Evidence_in_over_10_Isolates, c.Clinical_Evidence_Is_Affiliated_with_Accession_Number, c.Breakpoint_Organization,
                                        c.Susceptibility_Method_for_Clinical_Isolates, c.Transformation_Experiment_Performed, c.Transformation_Is_Affiliated_with_Accession_Number,
                                        c.Transformation_Experiment_Source_URL, c.Transformant_Species, c.MIC_Value_for_Transformant,
                                        c.Fold_Change_in_Transformant, c.MIC_Interpretation_for_Tranformant, c.Hydrolysis_Is_Associated_with_Accession_Number,
                                        c.Hydrolysis_Results, c.Method_for_MIC_Values, c.Mutation, 
                                        c.Type_of_Genetic_Marker, c.Additional_Information, c.SCCmec_Element,
                                        c.Aliases, c.Resistant_Mechanism,
                                        d.Specific_Cephalosporin, d.Organism_Group, d.Submission_Type,
                                        d.Gene_Has_Multiple_Species_Entries, d.MIC_Interpretation_for_Clinical_Isolate, d.Susceptibility_Method_for_Transformant,
                                        d.Hydrolysis_Experiment_Source_URL,
                                        d.Alteration_needed_for_resistance, d.Strain, d.Clinical_or_lab_generated,
                                        d.Number_of_mutations_in_gene, d.Single_mutation_known_to_cause_resistance,
                                        d.Causitive_evidence_source, d.Deletion_Transformation_Point_mutation, d.Type_of_lab_generated_strain,
                                        d.Method_to_generate_mutant, d.Fungal_species_using_in_lab_experiment, d.MIC_method_for_causal_evidence,
                                        d.Fluconazole_transformant_or_knockout_MIC_value, d.Fold_change, d.Target_of_Transcription_Factor,
                                        d.Downstream_Effect,
                                        d.Resistance_phenotype,
                                        d.Nucleotide_Sequence, d.NCBI_Accession_Number_Protein, d.NCBI_Accession_Number_PubMed_link,
                                        d.Breakpoint_applied_for_MIC_of_evidence_associated_isolate, d.Additional_mutations_required_for_resistance,
                                        d.Gene_has_multiple_GenBank_entries
                               from ft as a
                               left join ft_2 as b on a.original_table = b.original_table and
                                                        a.row_num = b.row_num
                               left join ft_3 as c on a.original_table = c.original_table and
                                                        a.row_num = c.row_num
                               left join ft_4 as d on a.original_table = d.original_table and
                                                        a.row_num = d.row_num
                               ")
  
  
  
  write.csv(final_table, "FDA project 2021 and 2018 combined.csv")
  
  
