## load libraries ----
library(tidyverse)
library(readxl)
library(janitor)
library(DT)
library(ggpubr)
library(UpSetR)
library(colorDF)
library(ComplexHeatmap)
library(cowplot)
library(lemon)

## define color scheme ----
col.models <- c("#42B54099", "#7AA6DCFF", "#0073C2FF", "#4A6990FF","#003C67FF", "#EFC000FF", "#8F7700FF", "#374E5599", "#374E55FF")
model.names <- c("human", "ChatGPT", "Perplexity.ai", "Galactica", "BioMed LM", "CiVIC", "OncoKB", "clinical", "combined")
names(col.models) <- model.names

col.patients <- c("#E93F1B", "#53E91B", "#0073C2FF", "#E618DE","#003C67FF", "#EFC000FF", "#8F7700FF", "#374E5599", "#898980", "#F4ED13")
patient.ids <- c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10")
names(col.patients) <- patient.ids

## read data ----
data.all <- read_csv("data/RecommendationsClean.csv")

## prepare data ----
data.all %>% 
  filter(!model %in% c("CiVIC", "OncoKB")) %>% 
  select(model, recommendation, alteration) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = model, values_from = value, values_fill = 0) %>%
  make_comb_mat(mode = "intersect") -> data.combinations

data.combinations[comb_size(data.combinations) >= 5 & comb_size(data.combinations) <= 20] -> data.filtered_combinations
order.sets <- order(set_size(data.filtered_combinations), decreasing = TRUE)
order.cols <- col.models[set_name(data.filtered_combinations)]

model.names <- c(set_name(data.filtered_combinations)[order.sets], "CiVIC", "OncoKB", "clinical", "combined")

## Plot number of TOs per variant and their upset plot ----
panel_c <- data.all %>% 
  mutate(model = factor(model, rev(model.names))) %>% 
  filter(!model %in% c("CiVIC", "OncoKB")) %>% 
  select(model, recommendation, alteration) %>%
  distinct() %>% 
  count(model, alteration)

write.csv(panel_c, "tables/panel_c.csv")

ggboxplot(panel_c, x = "model", y = "n", fill = "model", rotate = T, size = 0.1,
            add.params = list(size = 0.1),
            add = "jitter", ylab = "# unique recommendations / alteration", xlab = "") +
  theme_pubclean(base_size = 8) + ylim(0, 20) +
  scale_fill_manual(values = col.models) +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "plain"))
ggsave("figures/nrRecommendationsPerVariant.pdf", width = 6, height = 2, units = "cm")


pdf("figures/overlapRecommendationsPerVariant.pdf", width = 5, height = 3, useDingbats = T)
plot <- UpSet(data.filtered_combinations, 
      comb_order = order(comb_size(data.filtered_combinations), decreasing = TRUE),
      set_order = order.sets,
      top_annotation = upset_top_annotation(data.filtered_combinations, add_numbers = T),
      right_annotation = upset_right_annotation(data.filtered_combinations, add_numbers = T, 
                                                gp = gpar(fill = order.cols)))
svg("figures/overlapRecommendationsPerVariant.svg", width = 5, height = 3)
print(plot)
dev.off()

## Plot upset plots containing the condensed clinical TOs and human TOs ----
data.all %>% 
  filter(!model %in% c("human", "CiVIC", "OncoKB")) %>% 
  group_by(alteration, patient) %>% 
  count(recommendation) %>% 
  arrange(-n) %>% 
  filter(n > 1) -> data.option1

data.all %>% 
  filter(!model %in% c("human", "CiVIC", "OncoKB")) %>% 
  group_by(patient) %>% 
  filter(str_detect(reference, "NCT") | !is.na(clinical_study)) %>% 
  select(alteration, recommendation, reference, clinical_study) %>% 
  distinct() -> data.option2

data.option1 %>% 
  select(-n) %>% 
  mutate(filter = "combined") %>% 
  bind_rows(data.all %>% filter(model == "human") %>% 
              select(patient, alteration, recommendation) %>% 
              mutate(filter = "human")) %>% 
  bind_rows(data.option2 %>% select(patient, alteration, recommendation) %>% 
              mutate(filter = "clinical")) -> data.filtering

data.filtering %>% 
  ungroup() %>% 
  select(-patient) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = filter, values_from = value, values_fill = 0) %>% 
  make_comb_mat(mode = "intersect") -> data.combinations_2

data.combinations_2[comb_size(data.combinations_2) <= 40] -> data.filtered_combinations_2
order.sets <- order(set_size(data.filtered_combinations_2),decreasing = TRUE)
order.cols <- col.models[set_name(data.filtered_combinations_2)]

pdf("figures/overlapSummaryPerVariant.pdf", width = 5, height = 2.5, useDingbats = T)
data.filtered_combinations_2 %>%
  UpSet(comb_order = order(comb_size(data.filtered_combinations_2), decreasing = TRUE),
        set_order = order.sets,
        top_annotation = upset_top_annotation(data.filtered_combinations_2, add_numbers = T),
        right_annotation = upset_right_annotation(data.filtered_combinations_2, add_numbers = T,
                                                  gp = gpar(fill = order.cols)))
dev.off()

## Plot TOs per prompt type ----
panel_e <- data.all %>%
  filter(!is.na(model) & !model %in% c("human", "CiVIC", "OncoKB")) %>%
  mutate(group = model) %>% 
  mutate(model := if_else(perplexity_ai_v1 == 1, "Perplexity.ai P1", model, missing = model)) %>%
  mutate(model := if_else(perplexity_v2 == 1, "Perplexity.ai P2", model, missing = model)) %>%
  mutate(model := if_else(galactica_v1 == 1, "Galactica P1", model, missing = model)) %>%
  mutate(model := if_else(galactica_v2 == 1, "Galactica P2", model, missing = model)) %>%
  mutate(model := if_else(bio_med_lm_v1 == 1, "BioMed LM P1", model, missing = model)) %>%
  mutate(model := if_else(bio_med_lm_v2 == 1, "BioMed LM P2", model, missing = model)) %>%
  mutate(model := if_else(bio_med_lm_v3 == 1, "BioMed LM P3", model, missing = model)) %>%
  mutate(model := if_else(bio_med_lm_v4 == 1, "BioMed LM P4", model, missing = model)) %>% 
  select(group, model, recommendation, alteration) %>% 
  distinct() %>% 
  count(group, model, alteration)

write.csv(panel_e, "tables/panel_e.csv")

ggboxplot(panel_e, x = "model", y = "n", rotate = T, width=0.3, size = 0.3, outlier.shape = NA,
            ylab = "# unique recommendations / alteration", xlab = "") +
  geom_jitter(aes(col = group), width = .2, height = 0.1, size = 0.1) + 
  theme_pubclean(base_size = 8) + ylim(0, 20) +
  scale_fill_manual(values = col.models) +
  scale_color_manual(values = col.models) +
  scale_x_discrete(limits = rev) +
  theme(legend.position = "none",
        axis.title.x = element_text(face = "plain")) -> plot
ggsave("figures/nrRecommendationsPerPrompt.pdf", width = 6, height = 5, units = "cm")
svg("figures/nrRecommendationsPerPrompt.svg", width = 6, height = 5)
print(plot)
dev.off()

## Plot how many references are given for TOs with an annotated clinical phase I/II/III and how many NCT IDs exist  ----
data.all %>% 
  select(alteration, model, reference) %>% 
  distinct() %>% 
  mutate(reference = str_split(reference, ",")) %>% 
  unnest(reference) %>% 
  mutate(reference = str_squish(reference)) %>% 
  filter(startsWith(reference, "NCT")) %>% 
  pull(reference) %>% unique() -> suggestedNCT

names(suggestedNCT) <- suggestedNCT

# print(suggestedNCT)

library(httr)
library(jsonlite)
baseUrl <- "https://clinicaltrials.gov/api/query/study_fields?expr="
formatUrl <- "&fields=NCTId%2CBriefTitle%2CCondition%2CPhase%2COverallStatus%2CStartDate&min_rnk=1&max_rnk=&fmt=json"

lapply(suggestedNCT, function(x){
  API_URL <- paste0(baseUrl, x, formatUrl)
  raw_data <- GET(API_URL)
  if(raw_data$status_code == 200){
    resData <- fromJSON(rawToChar(raw_data$content), flatten = TRUE)
    return(resData$StudyFieldsResponse$StudyFields)
  }
}) %>% 
  bind_rows(.id = "query") %>% 
  as_tibble() -> res

res %>% 
  unnest(c(NCTId, BriefTitle, OverallStatus)) %>% 
  right_join(enframe(suggestedNCT, name = "query")) %>% 
  select(query, NCTId) %>% 
  filter(is.na(NCTId) | (NCTId == query)) %>% 
  mutate(given = NCTId == query) %>% 
  replace_na(list(given = FALSE)) %>% 
  mutate(class = "NCT") -> data.nct

data.all %>% 
  filter(!is.na(clinical_study)) %>% 
  select(recommendation, reference, reference2, clinical_study) %>% 
  mutate(given = 2 - (is.na(reference) + is.na(reference2)) > 0) %>% 
  select(given) %>% 
  mutate(class = "clinical") %>% 
  bind_rows(data.nct) -> available_references

write.csv(available_references, "tables/available_references.csv")
  
available_references  %>% 
  ggplot(aes(x = class, fill = given)) + geom_bar(color = "black") + 
  rotate() + 
  scale_fill_manual(values = c("lightgray", "darkgray")) + 
  ylim(c(0,115)) +
  theme_pubclean(base_size = 8) + 
  theme(legend.position = "none") -> plot
ggsave("figures/availableReferences.pdf", width = 4.5, height = 3, units = "cm")
svg("figures/availableReferences.svg", width = 4.5, height = 3)
print(plot)
dev.off()

available_references  %>% 
  ggplot(aes(x = class, fill = given)) + geom_bar(color = "black") + 
  rotate() + 
  scale_fill_manual(values = c("lightgray", "darkgray")) + 
  ylim(c(0,115)) +
  theme_pubclean(base_size = 10) + 
  theme(legend.position = "none") -> plot
svg("figures/availableReferences_adjusted.svg", width = 4.5, height = 2.5)
print(plot)
dev.off()


data.filtering %>% 
  bind_rows(data.all %>% filter(model %in% c("CiVIC", "OncoKB")) %>% 
            select(patient, alteration, recommendation, model)) %>% 
  ungroup() %>% 
  mutate(filter = ifelse(is.na(filter), model, filter)) %>% 
  select(filter, recommendation, alteration) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = filter, values_from = value, values_fill = 0) %>%
  make_comb_mat(mode = "intersect") -> data.combinations_3

data.combinations_3[comb_size(data.combinations_3) <= 40 & comb_size(data.combinations_3) > 5] -> data.filtered_combinations_3
order.sets <- order(set_size(data.filtered_combinations_3), decreasing = TRUE)
order.cols <- col.models[set_name(data.filtered_combinations_3)]

pdf("figures/overlapDataBasePerVariant.pdf", width = 4, height = 2.5, useDingbats = T)
data.filtered_combinations_3 %>%
  UpSet(comb_order = order(comb_size(data.filtered_combinations_3), decreasing = TRUE),
        set_order = order.sets,
        top_annotation = upset_top_annotation(data.filtered_combinations_3, add_numbers = T),
        right_annotation = upset_right_annotation(data.filtered_combinations_3, add_numbers = T,
                                                  gp = gpar(fill = order.cols)))
dev.off()

# Analysis of references per LLM

data.all %>% 
  select(alteration, model, reference) %>% 
  distinct() %>% 
  mutate(reference = str_split(reference, ",")) %>% 
  unnest(reference) %>% 
  mutate(reference = str_squish(reference)) %>% 
  filter(startsWith(reference, "NCT")) %>%
  unique() %>%
  mutate(query = reference) %>%
  inner_join(data.nct, by = "query") -> modelNCT

write.csv(modelNCT, "tables/available_model_references.csv")

## Plot F1 scores treating human annotation as gold standard ----
data.all %>% 
  select(patient, alteration, model, recommendation) %>% 
  distinct() %>% 
  mutate(value = 1) %>%
  pivot_wider(names_from = model, values_from = value, values_fill = 0) %>% 
  pivot_longer(-c("patient", "alteration", "recommendation", "human")) -> data.models

data.filtering %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = filter, values_from = value, values_fill = 0) %>% 
  pivot_longer(c("combined", "clinical")) %>% 
  bind_rows(data.models) %>% 
  group_by(patient, name) %>% 
  count(value, human) %>% 
  mutate(classification = ifelse(value == human & human == 1, "TP", NA)) %>% 
  mutate(classification = ifelse(value == human & human == 0, "TN", classification)) %>% 
  mutate(classification = ifelse(value != human & human == 1, "FN", classification)) %>% 
  mutate(classification = ifelse(value != human & human == 0, "FP", classification)) %>% 
  select(classification, n) %>% 
  pivot_wider(names_from = classification, values_from = n, values_fill = 0) -> data.tmp

data.tmp %>% 
  mutate(precision = TP/(TP+FP)) %>% 
  mutate(recall = TP/(TP+FN)) %>% 
  mutate(f1 = 2*precision*recall/(precision + recall)) -> data.pr

data.all %>% 
  ungroup() %>% 
  select(alteration, model, recommendation) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = model, values_from = value, values_fill = 0) %>% 
  pivot_longer(-c("alteration", "recommendation", "human")) -> pr.models

data.filtering %>% 
  ungroup() %>% 
  select(-patient) %>% 
  distinct() %>% 
  mutate(value = 1) %>% 
  pivot_wider(names_from = filter, values_from = value, values_fill = 0) %>% 
  pivot_longer(c("combined", "clinical")) %>% 
  bind_rows(pr.models) %>% 
  group_by(name) %>% 
  count(value, human) %>% 
  mutate(classification = ifelse(value == human & human == 1, "TP", NA)) %>% 
  mutate(classification = ifelse(value == human & human == 0, "TN", classification)) %>% 
  mutate(classification = ifelse(value != human & human == 1, "FN", classification)) %>% 
  mutate(classification = ifelse(value != human & human == 0, "FP", classification)) %>% 
  select(classification, n) %>% 
  pivot_wider(names_from = classification, values_from = n, values_fill = 0) %>% 
  mutate(precision = TP/(TP+FP)) %>% 
  mutate(recall = TP/(TP+FN)) %>% 
  mutate(f1 = 2*precision*recall/(precision + recall)) -> pr.overall

pr.overall %>% 
  pivot_longer(precision:f1, names_to = "classification") %>% 
  mutate(classification = factor(classification, levels = c("precision", "recall", "f1"))) %>%
  mutate(model = factor(name, levels = rev(model.names))) %>%
  filter(!model %in% c("CiVIC", "OncoKB")) -> pr.overall2

pr.overall %>% 
  pivot_longer(precision:f1, names_to = "classification") %>% 
  mutate(classification = factor(classification, levels = c("precision", "recall", "f1"))) %>%
  mutate(model = factor(name, levels = rev(model.names))) -> pr.overall3

# print(data.pr)

panel_g <- data.pr %>% 
#  bind_rows(pr.overall) %>% 
  pivot_longer(precision:f1, names_to = "classification") %>% 
  mutate(classification = factor(classification, levels = c("precision", "recall", "f1"))) %>%
  mutate(model = factor(name, levels = rev(model.names))) %>%
  mutate(patient = factor(patient, levels = rev(patient.ids))) %>%
  filter(!model %in% c("CiVIC", "OncoKB"))

# print(panel_g)

#  mutate(model = factor(name, levels = rev(c("ChatGPT", "Perplexity.ai", "Galactica", "BioMed LM", "CiVIC", "clinical evidence", "combined")))) %>% 
#  filter(model != "CiVIC") %>% 

# Precision and recall per model
# plot <- ggscatter(panel_g, x = "model", y = "value", color = "model", size = 1,
#             rotate = T, facet.by = "classification",
#             ncol = 1, panel.labs = list(classification = c("precision", "recall", "F1-score"))) +
#   scale_color_manual(values = col.models) +
#   geom_point(data = pr.overall2, col = "darkred", size = 2, shape = 24, fill = "darkred") +
#   theme_pubclean(base_size = 12) + 
#   theme(legend.position = "none") +
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# Precision and recall per patient
plot <- ggscatter(panel_g, x = "model", y = "value", color = "patient", size = 1,
            rotate = T, facet.by = "classification",
            ncol = 1, panel.labs = list(classification = c("precision", "recall", "F1-score"))) +
  scale_color_manual(values = col.patients) +
  geom_point(data = pr.overall2, col = "darkred", size = 2, shape = 24, fill = "darkred") +
  theme_pubclean(base_size = 12) + 
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

ggsave("figures/precisionRecall.pdf", width = 5, height = 6, units = "cm")

svg("figures/precision_recall.svg", width = 5, height = 6)
print(plot)
dev.off()

data.pr %>% 
  pivot_longer(f1, names_to = "classification") %>% 
  mutate(classification = factor(classification, levels = c("f1"))) %>%
  mutate(model = factor(name, levels = rev(model.names))) %>%
  mutate(patient = factor(patient, levels = rev(patient.ids))) %>%
  filter(!model %in% c("CiVIC", "OncoKB")) -> f1_score_panel

# print(f1_score_panel)

pr.overall %>% 
  pivot_longer(f1, names_to = "classification") %>% 
  mutate(classification = factor(classification, levels = c("f1"))) %>%
  mutate(model = factor(name, levels = rev(model.names))) %>%
  filter(!model %in% c("CiVIC", "OncoKB")) -> f1.overall

# print(f1.overall)

# Check the number of levels in panel.labs
# print(nlevels(list(classification = c("F1-score"))))

# Check the number of levels in the data
# print(sapply(f1_score_panel, nlevels))
# print(f1_score_panel)

# F1-score only (coloring according to model)
# plot <- ggscatter(f1_score_panel, x = "model", y = "value", color = "model", size = 2,
#             rotate = T, facet.by = "classification",
#             ncol = 1, panel.labs = list(classification = c("F1-score"))) +
#   scale_color_manual(values = col.models) +
#   geom_point(data = f1.overall, col = "darkred", size = 3, shape = 24, fill = "darkred") +
#   theme_pubclean(base_size = 15) + 
#   theme(legend.position = "none") +
#   theme(axis.title.x = element_blank(), axis.title.y = element_blank())

# F1-score only (coloring according to patient)
plot <- ggscatter(f1_score_panel, x = "model", y = "value", color = "patient", size = 2,
            rotate = T, facet.by = "classification",
            ncol = 1, panel.labs = list(classification = c("F1-score"))) +
  scale_color_manual(values = col.patients) +
  geom_point(data = f1.overall, col = "darkred", size = 3, shape = 24, fill = "darkred") +
  theme_pubclean(base_size = 15) + 
  theme(legend.position = "none") +
  theme(axis.title.x = element_blank(), axis.title.y = element_blank())

svg("figures/f1_score.svg", width = 5, height = 2)
print(plot)
dev.off()

write.csv(panel_g, "tables/panel_g.csv")

write.csv(pr.overall2, "tables/panel_g_combined.csv")

write.csv(pr.overall3, "tables/panel_g_combined_all.csv")

panel_g_combined <- data.pr %>% 
  group_by(name) %>% 
  summarise(median = median(f1, na.rm = T))

## Plot MTB survey results ----
meta.order <- tribble(
  ~patient, ~human, ~combined, ~clinical,
  1, 3, 2, 1,
  2, 2, 3, 1,
  3, 2, 1, 3,
  4, 1, 3, 2,
  5, 1, 3, 2,
  6, 3, 1, 2,
  7, 1, 2, 3,
  8, 2, 3, 1, 
  9, 2, 3, 1, 
  10, 2, 3, 1,  
)

meta.order %>% 
  pivot_longer(-patient, values_to = "filter") %>% 
  rowwise() %>% 
  mutate(filter = str_c("SQ00", filter, collapse = "")) -> meta.order


all.survey <- read_csv("data/SurveyResponsesAll.csv")
all.survey %>% 
  filter(!is.na(value)) %>% 
  filter(str_detect(name, "Q03\\[")) %>% 
  mutate(name = str_remove(name, "Q03")) %>% 
  separate(name, into = c("patient", "filter"), sep = "\\[") %>% 
  mutate(filter = str_remove(filter, "\\]")) %>% 
  mutate(value = parse_number(value)) %>% 
  mutate(patient = parse_number(patient)) %>% 
  left_join(meta.order, by = c("patient", "filter")) %>% 
  mutate(patient = ifelse(patient < 10, paste0("0", patient), patient)) %>% 
  mutate(patient =  paste("patient", patient)) %>% 
  mutate(filter = parse_number(filter)) %>% 
  ggdotplot(x = "filter", y = "value", rotate = T,
            fill = "name", color = "name", xlab = "option", size = 1.5)  +
  ylim(0,10) +
  facet_wrap(~patient, ncol = 2) +
  scale_fill_manual(values = col.models) + scale_color_manual(values = col.models) +
  theme_pubclean(base_size = 10)+ 
  theme(legend.position = "right") -> plot
ggsave("figures/surveyResult.pdf", width = 9, height = 10, units = "cm")
svg("figures/surveyResult.svg", width = 9, height = 10)
print(plot)
dev.off()

all.survey %>% 
  filter(!is.na(value)) %>% 
  filter(str_detect(name, "G.*Q00$")) %>% 
  mutate(name = str_remove(name,  "Q00")) %>% 
  mutate(patient = parse_number(name)) %>% 
  select(id, patient, value) %>% 
  arrange(patient) %>% 
  DT::datatable()

all.survey %>% 
  filter(!is.na(value)) %>% 
  filter(str_detect(name, "G.*Q0[12]$")) %>% 
  separate(name, into = c("name", "question"), sep = "Q") %>% 
  mutate(patient = parse_number(name)) %>% 
  filter(patient <= 10, patient > 0) %>% 
  select(id, patient, question, value) %>% 
  pivot_wider(names_from = question) %>% 
  rename(Option = `01`) %>% 
  rename(`Why?` = `02`) %>% 
  count(patient, Option)

all.survey %>% filter(!is.na(lastpage)) %>% filter(str_detect(name, "G08")) %>% View()
