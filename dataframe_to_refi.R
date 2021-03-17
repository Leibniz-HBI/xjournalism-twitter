library(xml2)
library(magrittr)
library(dplyr)
library(pals)
library(zip)
library(uuid)

dataframe_to_refi <- function(x, export_codes, file, schema_file = "refi-project.xsd") {
  
  ilcm_user_uuid <- uuid::UUIDgenerate()
  ilcm_user_name <- "iLCM export user"
  current_date_time <- "2019-08-09T12:00:00Z"
  
  tmp_path <- paste0(gsub("\\\\", "/", tempfile("refi-qda-")), "/")
  tmp_path_sources <- paste0(tmp_path, "Sources/")
  dir.create(tmp_path_sources, recursive = T)
  
  root <- xml2::xml_new_document() %>% 
    xml2::xml_add_child("Project", 
                        "xmlns:xsd" = "http://www.w3.org/2001/XMLSchema",
                        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                        "name" = "FB Social Media",
                        "origin" = "iLCM REFI Export",
                        "basePath" = "",
                        "xmlns" = "urn:QDA-XML:project:1.0",
                        "creatingUserGUID" = ilcm_user_uuid
    )
  
  root %>% 
    xml2::xml_add_child("Users") %>% 
    xml2::xml_add_child("User", "guid" = ilcm_user_uuid, "name" = ilcm_user_name)
  
  
  codes_dictionary <- list()
  codes <- root %>%
    xml2::xml_add_child("CodeBook") %>% 
    xml2::xml_add_child("Codes")
  
  for (code in export_codes) {
    
    codes_dictionary[[code]] <- list()
    
    # ensure factors
    if (class(x[, code]) != "factor") {
      x[, code] <- factor(x[, code])
    }
    current_codes <- levels(x[, code])
    
    toplevel_uuid <- uuid::UUIDgenerate()
    toplevel_code <- codes %>%
      xml2::xml_add_child("Code", 
                          "guid" = toplevel_uuid, 
                          "name" = code, 
                          "isCodable" = "false")
    
    i <- 1
    colors <- rep(alphabet(), length.out = length(current_codes))
    
    for (current_code in current_codes) {
      
      sublevel_uuid <- uuid::UUIDgenerate()
      toplevel_code %>%
        xml2::xml_add_child("Code", 
                            "guid" = sublevel_uuid, 
                            "name" = current_code, 
                            "isCodable" = "true", 
                            "color" = colors[i])
      
      codes_dictionary[[code]][[current_code]] <- sublevel_uuid
      
      i <- i + 1
      
    }
    
  }
  
  sources <- root %>%
    xml2::xml_add_child("Sources")
  
  for (i in 1:nrow(x)) {
    
    source_uuid <- uuid::UUIDgenerate()
    source_text <- x$text[i]
    source_name <- x$doc_id[i]
    
    writeLines(source_text, paste0(tmp_path_sources, source_uuid, ".txt"), useBytes = T)
    
    text_source <- sources %>%
      xml2::xml_add_child("TextSource", 
                          "guid" = source_uuid, "name" = source_name,
                          "plainTextPath" = paste0("internal://", source_uuid, ".txt"),
                          "creatingUser" = ilcm_user_uuid,
                          "creationDateTime" = current_date_time)
    
    plaintext_selection_uuid <- uuid::UUIDgenerate()
    plaintext_selection <- text_source %>% 
      xml2::xml_add_child("PlainTextSelection", 
                          "guid" = plaintext_selection_uuid,
                          "name" = "",
                          "startPosition" = "0",
                          "endPosition" = nchar(source_text) + 1,
                          "creatingUser" = ilcm_user_uuid,
                          "creationDateTime" = current_date_time
      )
    
    for (code in export_codes) {
      
      coding_uuid <- uuid::UUIDgenerate()
      code_ref_uuid <- codes_dictionary[[code]][[x[i, code]]]
      plaintext_selection %>%
        xml2::xml_add_child("Coding", 
                            "guid" = coding_uuid,
                            "creatingUser" = ilcm_user_uuid,
                            "creationDateTime" = current_date_time
        ) %>%
        xml2::xml_add_child("CodeRef", "targetGUID" = code_ref_uuid)
    }
    
    
  }
  
  # write project qde file
  project_file <- paste0(tmp_path, "project.qde")
  xml2::write_xml(root, project_file)
  
  # validate project qde file
  doc <- xml2::read_xml(project_file)
  schema <- xml2::read_xml(schema_file)
  validation_result <- xml_validate(doc, schema)
  
  # pack to qdpx format
  tmp_wd <- getwd()
  setwd(tmp_path)
  zip::zipr(file.path(tmp_wd, file), c("project.qde", "Sources"), recurse = T)
  setwd(tmp_wd)
  
  # delete tmp dir
  unlink(gsub("/$", "", tmp_path, perl = T), recursive = T)
  
  return(validation_result)
  
}




# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~




dataframe_to_refi_group_by <- function(x, export_codes, file, group_var = NULL, delete_emojies = T, schema_file = "refi-project.xsd") {
  
  require(dplyr)
  
  # enforce UTF-8 encoding
  x$text <- stringi::stri_encode(x$text, "", "UTF-8")
  # enforce unix line endings
  x$text <- stringi::stri_replace_all_fixed(x$text, "\r\n", "\n")
  
  ilcm_user_uuid <- uuid::UUIDgenerate()
  ilcm_user_name <- "iLCM export user"
  current_date_time <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  
  tmp_path <- paste0(gsub("\\\\", "/", tempfile("refi-qda-")), "/")
  tmp_path_sources <- paste0(tmp_path, "Sources/")
  dir.create(tmp_path_sources, recursive = T)
  
  root <- xml2::xml_new_document() %>% 
    xml2::xml_add_child("Project", 
                        "xmlns:xsd" = "http://www.w3.org/2001/XMLSchema",
                        "xmlns:xsi" = "http://www.w3.org/2001/XMLSchema-instance",
                        "name" = "FB Social Media",
                        "origin" = "iLCM REFI Export",
                        "basePath" = "",
                        "xmlns" = "urn:QDA-XML:project:1.0",
                        "creatingUserGUID" = ilcm_user_uuid
    )
  
  root %>% 
    xml2::xml_add_child("Users") %>% 
    xml2::xml_add_child("User", "guid" = ilcm_user_uuid, "name" = ilcm_user_name)
  
  
  codes_dictionary <- list()
  codes <- root %>%
    xml2::xml_add_child("CodeBook") %>% 
    xml2::xml_add_child("Codes")
  
  for (code in export_codes) {
    
    codes_dictionary[[code]] <- list()
    
    # ensure factors
    if (class(x[, code]) != "factor") {
      x[, code] <- factor(x[, code])
    }
    current_codes <- levels(x[, code])
    
    toplevel_uuid <- uuid::UUIDgenerate()
    toplevel_code <- codes %>%
      xml2::xml_add_child("Code", 
                          "guid" = toplevel_uuid, 
                          "name" = code, 
                          "isCodable" = "false")
    
    i <- 1
    colors <- pals::alphabet()
    
    for (current_code in current_codes) {
      
      sublevel_uuid <- uuid::UUIDgenerate()
      toplevel_code %>%
        xml2::xml_add_child("Code", 
                            "guid" = sublevel_uuid, 
                            "name" = current_code, 
                            "isCodable" = "true", 
                            "color" = colors[i])
      
      codes_dictionary[[code]][[current_code]] <- sublevel_uuid
      
      i <- ifelse(i >= length(colors), 1, i + 1)
      
    }
    
  }
  
  sources <- root %>%
    xml2::xml_add_child("Sources")
  
  
  # 1. group by
  # 2. iteration over group elements: multiple PlainTextSelections and resp. Codings per TextSource
  # - doc_id, text, code1, code2, pseudoc_text, nchar, n_position
  
  if (!is.null(group_var)) {
    
    # rows in x get grouped to documents
    separator <- "\n\n--\n\n"
    separator_length <- nchar(separator, type = "chars")
    
    if (delete_emojies) {
      # remove emojies
      x$text <- stringi::stri_replace_all_regex(x$text, "[\U0001F1E0-\U0001F1FF\U0001F300-\U0001F5FF\U0001F600-\U0001F64F\U0001F680-\U0001F6FF\U0001F700-\U0001F77F\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\U0001FA00-\U0001FA6F\U0001FA70-\U0001FAFF\U00002702-\U000027B0]", "")
    }
    
    # infer stand off annotation
    x <- x %>%
      arrange(group_var) %>%
      mutate(length = nchar(text, type = "chars") + separator_length) %>%
      group_by_at(group_var) %>%
      mutate(
        pseudoc = paste(text, collapse = separator),
        startPosition = cumsum(length) - length,
        endPosition = cumsum(length) - separator_length
      )

    for (i in 1:nrow(x)) {
      
      elem <- x[i, ]
      
      if (elem$startPosition == 0) {
        
        # beginning of new grouped document
        
        source_uuid <- uuid::UUIDgenerate()
        source_text <- elem$pseudoc
        source_name <- paste0(group_var, " ", unlist(elem[group_var]))
        
        stringi::stri_write_lines(source_text, paste0(tmp_path_sources, source_uuid, ".txt"), sep = "\n")
        
        text_source <- sources %>%
          xml2::xml_add_child("TextSource", 
                              "guid" = source_uuid, 
                              "name" = source_name,
                              "plainTextPath" = paste0("internal://", source_uuid, ".txt"),
                              "creatingUser" = ilcm_user_uuid,
                              "creationDateTime" = current_date_time)
        
      }
      
      plaintext_selection_uuid <- uuid::UUIDgenerate()
      plaintext_selection <- text_source %>% 
        xml2::xml_add_child("PlainTextSelection", 
                            "guid" = plaintext_selection_uuid,
                            "name" = "",
                            "startPosition" = elem$startPosition,
                            "endPosition" = elem$endPosition,
                            "creatingUser" = ilcm_user_uuid,
                            "creationDateTime" = current_date_time
        )
      
      for (code in export_codes) {
        coding_uuid <- uuid::UUIDgenerate()
        current_code <- unlist(elem[code])
        if (!is.na(current_code)) {
          code_ref_uuid <- codes_dictionary[[code]][[current_code]]
          plaintext_selection %>%
            xml2::xml_add_child("Coding", 
                                "guid" = coding_uuid,
                                "creatingUser" = ilcm_user_uuid,
                                "creationDateTime" = current_date_time
            ) %>%
            xml2::xml_add_child("CodeRef", "targetGUID" = code_ref_uuid)
        }
      }
    }
    
    # ------------------
    
  } else {
    
    # every row in x is a separate document
    
    if (delete_emojies) {
      # remove emojies
      x$text <- stringi::stri_replace_all_regex(x$text, "[\U0001F1E0-\U0001F1FF\U0001F300-\U0001F5FF\U0001F600-\U0001F64F\U0001F680-\U0001F6FF\U0001F700-\U0001F77F\U0001F780-\U0001F7FF\U0001F800-\U0001F8FF\U0001F900-\U0001F9FF\U0001FA00-\U0001FA6F\U0001FA70-\U0001FAFF\U00002702-\U000027B0]", "")
    }
    
    for (i in 1:nrow(x)) {
      
      source_uuid <- uuid::UUIDgenerate()
      source_text <- x$text[i]
      source_name <- x$doc_id[i]
      
      stringi::stri_write_lines(source_text, paste0(tmp_path_sources, source_uuid, ".txt"), sep = "\n")
      
      text_source <- sources %>%
        xml2::xml_add_child("TextSource", 
                            "guid" = source_uuid, "name" = source_name,
                            "plainTextPath" = paste0("internal://", source_uuid, ".txt"),
                            "creatingUser" = ilcm_user_uuid,
                            "creationDateTime" = current_date_time)
      
      plaintext_selection_uuid <- uuid::UUIDgenerate()
      plaintext_selection <- text_source %>% 
        xml2::xml_add_child("PlainTextSelection", 
                            "guid" = plaintext_selection_uuid,
                            "name" = "",
                            "startPosition" = "0",
                            "endPosition" = nchar(source_text, type = "char"),
                            "creatingUser" = ilcm_user_uuid,
                            "creationDateTime" = current_date_time
        )
      
      for (code in export_codes) {
        coding_uuid <- uuid::UUIDgenerate()
        code_ref_uuid <- codes_dictionary[[code]][[x[i, code]]]
        plaintext_selection %>%
          xml2::xml_add_child("Coding", 
                              "guid" = coding_uuid,
                              "creatingUser" = ilcm_user_uuid,
                              "creationDateTime" = current_date_time
          ) %>%
          xml2::xml_add_child("CodeRef", "targetGUID" = code_ref_uuid)
      }
    }
  }
  
  # write project qde file
  project_file <- paste0(tmp_path, "project.qde")
  xml2::write_xml(root, project_file)
  
  # validate project qde file
  doc <- xml2::read_xml(project_file)
  schema <- xml2::read_xml(schema_file)
  validation_result <- xml2::xml_validate(doc, schema)
  
  # pack to qdpx format
  tmp_wd <- getwd()
  setwd(tmp_path)
  zip::zipr(file.path(tmp_wd, file), c("project.qde", "Sources"), recurse = T)
  setwd(tmp_wd)
  
  # delete tmp dir
  unlink(gsub("/$", "", tmp_path, perl = T), recursive = T)
  
  return(validation_result)
  
}

# options(stringsAsFactors = F)
# 
# test_text_df <- data.frame(
#   doc_id = 1:5,
#   text = c("Mein erster Text", "Die 2te", "Dre drei drei", "Keine Ahnung", "nur Fünf"),
#   parent = c("1", "1", "1", "2", "2"),
#   mycode = c("hs", "hs", "cs", "cs", "hs")
# )
# 
# dataframe_to_refi_group_by(test_text_df, c("mycode"), "test.qdpx", group_var = "parent")
