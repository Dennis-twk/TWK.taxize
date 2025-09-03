### Script name: 清洗物种名称，处理常见的拼写错误和格式问题，获取上级分类阶元。

### Date Created:2025-09-03


# 整个逻辑：
# 1）先在NCBI上检索物种名，获取taxid和分类信息；
# 2）对NCBI没有找到的物种名在GBIF上检索；
# 3）可选，GBIF上并没有总科信息，需要将科通过NCBI检索总科；
# 4）对GBIF没有找到的物种名通过属去NCBI查询上级分类阶元。

# Clean the environment
rm(list = ls());gc()

# load packages
library(taxize)
library(openxlsx)
library(dplyr)
library(stringr)

# 0) 预处理 ------------------------------------------------------------------
# 读取含有"input.name"列的excle，去除所有字符型列中开头/结尾的空格
data <- read.xlsx("input/测试_标准.xlsx", sheet = "鳞翅目") %>%
  pull(`input.name`) %>%        # 只拿这一列
  trimws()                      # 去首尾空格
# 提醒含有英文句点.
dot_idx <- str_which(data, fixed("."))

if (length(dot_idx) > 0) {
  warn_msg <- sprintf(
    "以下行包含英文句点（.）：\n%s",
    paste(sprintf("第 %d 行: \"%s\"", dot_idx, data[dot_idx]), collapse = "\n"))
  warning(warn_msg)  # 输出到控制台
  writeLines(warn_msg, "output/data_period_warnings.txt")  # 写入文件
  cat("警告信息已保存至: output/data_period_warnings.txt", flush = TRUE)
}

# sink()

data_splist <- unique(data) # 去重一下避免重复


# 1）先在NCBI上检索 ---------------------------------------------------------------
## 由于后续需要分子数据，所以物种名的清洗优先使用NCBI数据库

# NCBI数据库载入密钥可以加快检索速度，注意不能有空格，改成你自己的密钥：
## NCBI密钥获取链接： https://ncbiinsights.ncbi.nlm.nih.gov/2017/11/02/new-api-keys-for-the-e-utilities/
## 没有 API 密钥的情况下每秒 3 个请求，有API就每秒10个请求。
options(ENTREZ_KEY='651b9638111ab101bc031ca572fbd440a008')

# 开始重定向输出到文件
sink("output/ncbi_ids_output.txt", type = "output", append = FALSE)  # 覆盖模式写入

ids_NCBI <- taxize::get_ids(data_splist, db = 'ncbi')

sink() # 关闭标准输出重定向

out_id_ncbi <- data.frame(unlist(ids_NCBI)) # 得到物种在NCBI的taxid
out_id_ncbi <- na.omit(out_id_ncbi) #去除没找到taxid的物种行（NA的行）
out_id_ncbi <- data.frame(rownames(out_id_ncbi),out_id_ncbi[,1]) #建立输入的物种名和taxid的对应关系
out_id_ncbi$rownames.out_id_ncbi. <- gsub("ncbi.","", out_id_ncbi$rownames.out_id_ncbi.) #把上面物种名前面的ncbi.去掉
colnames(out_id_ncbi) <- c("input.name","ncbi.taxid")

# 输出多选和没有找到对应taxid的物种，后续重新检索
missing_ncbi <- setdiff(data_splist, out_id_ncbi$input.name)

classf_ncbi <- taxize::classification(out_id_ncbi$ncbi.taxid,db = 'ncbi') #问询的物种数量较多时（5000+）可能会报错：错误: Bad Request (HTTP 400) 可尝试使用外网
# save(classf_ncbi,file = 'output/classf_ncbi.Rdata') # 若数据量大，可以保存成R.data

## NCBI分类阶元的函数 --------------------------------------------------------------

lineage_ncbi <- function(x, rank0) {
  d0 <- paste0('NCBI.', rank0)
  for (i in seq_along(x)) {
    sub <- subset(x[[i]], rank == rank0, select = name)
    if (nrow(sub)) {
      d0 <- cbind(d0, sub)
      colnames(d0)[ncol(d0)] <- names(x)[i]
    }
  }
  d1 <- t(d0)
  d2 <- data.frame(rownames(d1), d1[, 1], stringsAsFactors = FALSE)
  d3 <- d2[-1, ]
  names(d3) <- c('ncbi.taxid', paste0('NCBI.', rank0))
  d3
}

## 一键返回全阶元的函数
# 注意：①NCBI动物的kingdom是Metazoa，不是Animalia
# 注意：②没有总科信息的类群，例如：植物，则将"superfamily"删去
lineage_all_ncbi <- function(x) {
  ranks <- c("kingdom","phylum","class","order",
             "superfamily","family","genus","species")
  # ranks <- c("kingdom","phylum","class","order",
  #            "family","genus","species")
  lst <- lapply(ranks, function(r) lineage_ncbi(x, r))
  Reduce(function(a, b) merge(a, b, by = "ncbi.taxid", all.x = TRUE),
         lst, init = out_id_ncbi)
}

all_ncbi <- lineage_all_ncbi(classf_ncbi)

openxlsx::write.xlsx(all_ncbi,'output/all_ncbi.xlsx',rowNames = F,colNames=T)

# 2.1）上一步找不到的再去GBIF检索 ---------------------------------------------------------
# GBIF不需要密钥
# 【错误于curl::curl_fetch_memory(x$url$url, handle = x$url$handle):
# Error in the HTTP2 framing layer [api.gbif.org]】
# 由于 taxize 包在调用 GBIF API 时使用了 HTTP/2，而 GBIF 的服务器或中间网络设备对 HTTP/2 支持不稳定，导致连接失败
# 出现这个错误就加上, curlopts = list(http_version = 2L)
sink("output/gbif_ids_output1.txt", type = "output", append = FALSE)

# 限定鳞翅目类群，跳过需要选择的类群，ask=FALSE
output <- capture.output({
  ids_GBIF <- taxize::get_ids(missing_ncbi, db = 'gbif', ask=FALSE, order = "Lepidoptera", curlopts = list(http_version = 2L))
}, type = "message") # 指定捕获“message”类型输出，这包括警告

# 获取警告信息
captured_warnings <- warnings()
warning_text <- capture.output(print(captured_warnings))

sink()

# 提取所有单引号内的内容
# 使用正则表达式匹配单引号内的文本
matches <- gregexpr("'(.*?)'", warning_text)
extracted_text <- unlist(regmatches(warning_text, matches))

# 移除提取结果两端的单引号
extracted_text <- gsub("^'|'$", "", extracted_text)

# 保存到文本文件
writeLines(extracted_text, "output/gbif_ids_more.txt")


# 2.2) 多个选项或者查不到的内容需要手动选择 -------------------------------------------------

sink("output/gbif_ids_output2.txt", type = "output", append = TRUE)

# 出现需要选择的选项，选择ACCEPTED的项，（一般的第一个，求准确率建议还是去官网查一下）
ids_GBIF_more <- taxize::get_ids(extracted_text, db = 'gbif', order = "Lepidoptera", curlopts = list(http_version = 2L))

sink()

ids_GBIF_combined <- c(ids_GBIF$gbif, ids_GBIF_more$gbif)

# 得到物种在GBIF的gbif.ID
out_id_gbif <- data.frame(species = names(ids_GBIF_combined),gbif_id = ids_GBIF_combined)
out_id_gbif <- na.omit(out_id_gbif) #去除没找到taxid的物种行（NA的行）
colnames(out_id_gbif) <- c("input.name","gbif.ID")

# 若报错可以重新运行这一步
# 错误于names(object) <- nm:
# 'names' attribute [2] must be the same length as the vector [1]
classf_gbif <- taxize::classification(out_id_gbif$gbif.ID,db = 'gbif')
# save(classf_gbif,file = 'output/classf_gbif.Rdata') # 若数据量大，可以保存成R.data

### GBIF分类阶元的函数---------------------------------------------------------------
lineage_gbif <- function(x, rank0) {
  d0 <- paste0('GBIF.', rank0)
  for (i in seq_along(x)) {
    sub <- subset(x[[i]], rank == rank0, select = name)
    if (NROW(sub)) {
      d0 <- cbind(d0, sub)
      colnames(d0)[ncol(d0)] <- names(x)[i]
    }
  }
  d1 <- t(d0)
  d2 <- data.frame(gbif.ID = rownames(d1)[-1],
                   tmp    = d1[-1, 1],
                   stringsAsFactors = FALSE)
  names(d2)[2] <- paste0('GBIF.', rank0)
  d2
}

lineage_all_gbif <- function(x) {
  ranks <- c("kingdom","phylum","class","order",
             "family","genus","species")     # GBIF 没有 superfamily
  lst   <- lapply(ranks, function(r) lineage_gbif(x, r))
  Reduce(function(a, b) merge(a, b, by = "gbif.ID", all.x = TRUE),
         lst, init = out_id_gbif)
}

# 调用
all_gbif <- lineage_all_gbif(classf_gbif)
openxlsx::write.xlsx(all_gbif,'output/all_gbif.xlsx',rowNames = F,colNames=T)

# 输出没有找到对应gbif.ID的物种
missing_gbif <- setdiff(missing_ncbi, out_id_gbif$input.name)
writeLines(missing_gbif, "output/missing_gbif.txt")

# 3）GBIF输出的结果没有总科信息，需要通过科去NCBI查询总科，不需要则跳过此步骤 ----------------------------------------
# 先提取GBIF输出的科名
data_family <- unique(all_gbif$GBIF.family)
# options(ENTREZ_KEY='651b9638111ab101bc031ca572fbd440a008')

# 注意，下面这一步因为有同属、科名的情况，所以需要手动选择数字，回车。
#  status  rank division scientificname commonname     uid genus species subsp modificationdate
# 1 active clade     bees     Anthophila       bees 3042114                     2025/02/27 00:00
# 2 active genus    moths     Anthophila             999306                     2011/04/04 00:00
ids_NCBI_superfamily <- taxize::get_ids(data_family, db = 'ncbi')

out_id_ncbi_superfamily <- data.frame(unlist(ids_NCBI_superfamily)) # 得到科在NCBI的taxid
out_id_ncbi_superfamily <- na.omit(out_id_ncbi_superfamily) #去除没找到taxid的物种行（NA的行）
out_id_ncbi_superfamily <- data.frame(rownames(out_id_ncbi_superfamily),out_id_ncbi_superfamily[,1]) #建立输入的物种名和taxid的对应关系
out_id_ncbi_superfamily$rownames.out_id_ncbi_superfamily. <- gsub("ncbi.","", out_id_ncbi_superfamily$rownames.out_id_ncbi_superfamily.) #把上面物种名前面的ncbi.去掉
colnames(out_id_ncbi_superfamily) <- c("input.name","ncbi.taxid")

# 输出没有找到对应taxid的物种，后续用其他数据库检索
missing_superfamily_ncbi <- setdiff(data_family, out_id_ncbi_superfamily$input.name)

classf_superfamily_ncbi <- taxize::classification(out_id_ncbi_superfamily$ncbi.taxid,db = 'ncbi') #问询的物种数量较多时（5000+）可能会报错：错误: Bad Request (HTTP 400) 可尝试使用外网
# save(classf,file = 'output/classf.Rdata')

### NCBI总科分类阶元的函数-----------------------
lineage_superfamily_ncbi <- function(x, rank0) {
  d0 <- paste0('NCBI.', rank0)
  for (i in seq_along(x)) {
    sub <- subset(x[[i]], rank == rank0, select = name)
    if (NROW(sub)) {
      d0 <- cbind(d0, sub)
      colnames(d0)[ncol(d0)] <- names(x)[i]
    }
  }
  d1 <- t(d0)
  d2 <- data.frame(ncbi.taxid = rownames(d1)[-1],
                   tmp        = d1[-1, 1],
                   stringsAsFactors = FALSE)
  names(d2)[2] <- paste0('NCBI.', rank0)
  d2
}

lineage_all_superfamily_ncbi <- function(x) {
  ranks <- c("kingdom","phylum","class","order",
             "superfamily","family")   # 这里只到 family，没有 genus/species
  lst   <- lapply(ranks, function(r) lineage_superfamily_ncbi(x, r))
  Reduce(function(a, b) merge(a, b, by = "ncbi.taxid", all.x = TRUE),
         lst, init = out_id_ncbi_superfamily)
}

# 调用
all_superfamily_ncbi <- lineage_all_superfamily_ncbi(classf_superfamily_ncbi)

#　在lineage_all2中新增一列GBIF.superfamily，根据lineage_all3中NCBI.family与lineage_all2中的GBIF.family相同的科名，获取对应的总科名
lineage_missing <- all_gbif %>%
  left_join(all_superfamily_ncbi %>% select(NCBI.family, NCBI.superfamily) %>%
      rename(GBIF.family = NCBI.family),by = "GBIF.family") # 将NCBI.family重命名为GBIF.family用于匹配

# 将NCBI.superfamily移动到GBIF.family前
lineage_missing <- lineage_missing %>%
  relocate(NCBI.superfamily, .before = ncol(.) - 3)

ncbi_part <- all_ncbi %>%
  transmute(
    input.name = input.name,
    kingdom    = NCBI.kingdom,
    phylum     = NCBI.phylum,
    class      = NCBI.class,
    order      = NCBI.order,
    superfamily= NCBI.superfamily,
    family     = NCBI.family,
    genus      = NCBI.genus,
    species    = NCBI.species,
    source     = "NCBI"
  )

gbif_part <- lineage_missing %>%
  transmute(
    input.name = input.name,
    kingdom    = GBIF.kingdom,
    phylum     = GBIF.phylum,
    class      = GBIF.class,
    order      = GBIF.order,
    superfamily= NCBI.superfamily,
    family     = GBIF.family,
    genus      = GBIF.genus,
    species    = GBIF.species,
    source     = "GBIF"
  )

combined_all <- bind_rows(ncbi_part, gbif_part) # NCBI动物的kingdom是Metazoa，不是Animalia
openxlsx::write.xlsx(combined_all,'output/combined_all.xlsx',rowNames = F,colNames=T)

# 4）上述NCBI和GBIF均找不到则通过属去NCBI找上级分类界元 ------------------------------------------------
# missing_superfamily_ncbi <- missing_ncbi
missing_superfamily_ncbi_df <- tibble(
  species = missing_superfamily_ncbi,
  input.name   = word(missing_superfamily_ncbi, 1)   # 取第一个空格前的单词(属名)
)

missingdata <- unique(missing_superfamily_ncbi_df) # 去重一下

# options(ENTREZ_KEY='651b9638111ab101bc031ca572fbd440a008')

# 这一步运行后，会有同属、科的物种，需要手动选择你想要的物种类群
ids_miss <- taxize::get_ids(missingdata$input.name, db = 'ncbi')

out_id_miss <- data.frame(unlist(ids_miss)) # 得到物种在NCBI的taxid
out_id_miss <- na.omit(out_id_miss) #去除没找到taxid的物种行（NA的行）
out_id_miss <- data.frame(rownames(out_id_miss),out_id_miss[,1]) #建立输入的物种名和taxid的对应关系
out_id_miss$rownames.out_id_miss. <- gsub("ncbi.","", out_id_miss$rownames.out_id_miss.) #把上面物种名前面的ncbi.去掉
colnames(out_id_miss) <- c("input.name","ncbi.taxid")

# 输出没有找到对应taxid的物种
## 但不建议后面用GBIF检索，因为GBIF不会输出物种类群，有可能动物划分到了植物等错误。
# 如果这一步剩的不多了可选择手动去GBIF搜
missing_ncbi_genus <- setdiff(missingdata$input.name, out_id_miss$input.name)

classf_ncbi_genus <- taxize::classification(out_id_miss$ncbi.taxid,db = 'ncbi')
# save(classf,file = 'output/classf.Rdata')

### 属以上分类阶元的函数-----------------------
lineage_miss <- function(x, rank0) {
  d0 <- paste0('NCBI.', rank0)
  for (i in seq_along(x)) {
    sub <- subset(x[[i]], rank == rank0, select = name)
    if (NROW(sub)) {
      d0 <- cbind(d0, sub)
      colnames(d0)[ncol(d0)] <- names(x)[i]
    }
  }
  d1 <- t(d0)
  d2 <- data.frame(ncbi.taxid = rownames(d1)[-1],
                   tmp        = d1[-1, 1],
                   stringsAsFactors = FALSE)
  names(d2)[2] <- paste0('NCBI.', rank0)
  d2
}


lineage_all_miss <- function(x) {
  ranks <- c("kingdom","phylum","class","order",
             "superfamily","family","genus")
  lst   <- lapply(ranks, function(r) lineage_miss(x, r))
  Reduce(function(a, b) merge(a, b, by = "ncbi.taxid", all.x = TRUE),
         lst, init = out_id_miss)
}

# 调用
all_miss<- lineage_all_miss(classf_ncbi_genus)

# 匹配回去
missing_superfamily_ncbi_combined <- missing_superfamily_ncbi_df %>%
  left_join(all_miss,
            by = c("input.name" = "NCBI.genus"))
openxlsx::write.xlsx(missing_superfamily_ncbi_combined,'output/missing_superfamily_ncbi_combined.xlsx',rowNames = F,colNames=T)



