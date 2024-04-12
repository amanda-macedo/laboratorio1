pacman::p_load(readxl,dplyr)

base <- read_xlsx('C:/Users/amand/OneDrive/Documentos/GitHub/laboratorio1/Respostas.xlsx')

base <- base[, -ncol(base)]
name_columns <- c(
  "genero",
  "outro",
  "idade",
  "estado_civil",
  "outro_estado_civil",
  "filhos",
  "qtd_filhos",
  "idade_filhos",
  "mora_com",
  "outro_mora_com",
  "escolaridade",
  "curso_formacao",
  "categoria",
  "cargo",
  "cargo_nivel1",
  "cargo_nivel2",
  "carga_horaria_semanal",
  "outro_carga_horaria_semanal",
  "unidade_lotacao",
  "cd_ou_fg",
  "campus",
  "ano_ingresso",
  "ingresso_cotas_pcd",
  "sentimento_local_trabalho",
  "deficiencia",
  "deficiencia_outro",
  "periodo_diagnostico",
  "causa_deficiencia",
  "uso_tecnologia_assistiva_recurso",
  "qual_tecnologia",
  "tecnologia_disponibilizada_ufsm",
  "como_consegue_obter_tecnologia",
  "necessita_acompanhamento/apoio_atividades_cotidianas_notrabalho",
  "se_sim_quais_atividades?",
  "acompanhamento/apoio_disponibilizado_ufsm?",
  "se_nao_como_consegue_obter",
  "como_considera_acessibilidade_arquitetonica_ufsm",
  "como_considera_acessibilidade_atitudinal_ufsm",
  "como_considera_acessibilidade_instrumental/digital_ufsm",
  "como_considera_acessibilidade_comunicacional_ufsm",
  "conhece_legislacao_br_sobre_acessibilidade",
  "objetivo_quando_fez_concurso_ufsm",
  "outro_objetivo_quando_fez_concurso_ufsm",
  "dificuldade_adaptacao_durante_ingresso_atividades_laborais_ufsm",
  "dificuldade_adaptacao_se_sim_como_foi_resolvido",
  "enfrentou_problema_relacionamento_chefia_eou_colegas",
  "problema_relacionamento_se_sim_como_foi_resolvido",
  "ja_foi_parte_reclamante_processo_administrativo_disciplinar_sindicancia_eou_reclamatoria_trabalhista_naufsm",
  "considera_ufsm_dispoe_recursos_humanos_materiais_suficientes_atender_necessidades_que_eventualmente_surjam_em_relacao_acessibilidade",
  "se_nao_relate_oq_considera_insuficiente",
  "voce_participa_associacao/sindicato/outros_grupos_luta_direitos_trabalhistas",
  "se_sim_qual_associacao/sindicato",
  "se_participa_sente_pautas_pcd_consideradas_em_acoes_da_associacao/sindicato",
  "espaco_comentarios"
)

# curso formacao tirei pq ha 62 categorias distintas
# cargo tirei pq n tem padrao na escrita (42 categorias distintas)
# unidade_lotacao variavel inutil p gente e 56 categorias distintas
# ia tratar a variavel mora_com e outro_mora_com mas sao categorias demais e estao confusas

# Remocao colunas
colnames(base) <- name_columns
base1 <- base[, !colnames(base) %in% c("idade_filhos", "cargo_nivel1", "cargo_nivel2",
                                       "se_nao_como_consegue_obter","curso_formacao","cargo",
                                       "unidade_lotacao","mora_com","outro_mora_com")]

# Tratamento carga horaria semanal
base1 <- mutate(base1, carga_horaria_semanal = ifelse(carga_horaria_semanal == 'Outra', outro_carga_horaria_semanal, carga_horaria_semanal))
base1$carga_horaria_semanal <- gsub("\\D", "", base1$carga_horaria_semanal)
base1 <- base1[, !colnames(base1) %in% c("outro_carga_horaria_semanal")]

# Tratamento estado civil
base1 <- mutate(base1, estado_civil = ifelse(estado_civil == 'Outro', 'Divorciado(a)', estado_civil))
base1 <- base1[, !colnames(base1) %in% c("outro_estado_civil")]

# Tratamento qtd filhos
base1 <- mutate(base1, qtd_filhos = ifelse(is.na(qtd_filhos), "0", qtd_filhos))

# tratamento tipo das variaveis
base1$genero <- as.factor(base1$genero)

# Genero x grau de satisfacao
# acessibilidade arquitetonica
table(base1$genero,base1$como_considera_acessibilidade_arquitetonica_ufsm)
prop.table(table(base1$genero))
prop.table(table(base1$genero,base1$como_considera_acessibilidade_arquitetonica_ufsm))*100

x <- xtabs(~genero + como_considera_acessibilidade_arquitetonica_ufsm, data = base1)
x <- x[, c("Ótimo", "Bom", "Regular", "Ruim", "P?ssimo")]
cols <- c("#445a14", "#778c43")

barplot(x,
        beside = TRUE,
        xlab = "Como considera a acessibilidade arquitetônica na UFSM?",
        ylab = "Frequência absoluta",
        col = cols)
legend("topright",
       legend = levels(base1$genero),
       fill = cols,
       bty = "n")
box(bty = "L")
title(main = "Avaliação da Acessibilidade Arquitetônica na UFSM por Gênero", font.main = 3)

# acessibilidade atitudinal
table(base1$genero,base1$como_considera_acessibilidade_atitudinal_ufsm)
prop.table(table(base1$genero,base1$como_considera_acessibilidade_atitudinal_ufsm))*100

x <- xtabs(~genero + como_considera_acessibilidade_atitudinal_ufsm, data = base1)
x <- x[, c("Ótimo", "Bom", "Regular", "Ruim", "Péssimo")]
cols <- c("#445a14", "#778c43")

barplot(x,
        beside = TRUE,
        xlab = "Como considera a acessibilidade atitudinal na UFSM?",
        ylab = "Frequência absoluta",
        col = cols)
legend("topright",
       legend = levels(base1$genero),
       fill = cols,
       bty = "n")
box(bty = "L")
title(main = "Avaliação da Acessibilidade Atitudinal na UFSM por Gênero", font.main = 3)








questoes <- c("como_considera_acessibilidade_comunicacional_ufsm",
              "como_considera_acessibilidade_atitudinal_ufsm",
              "como_considera_acessibilidade_instrumental_digital_ufsm",
              "como_considera_acessibilidade_arquitetonica_ufsm")
niveis <- c("otimo", "bom", "regular", "ruim", "pessimo")

acessibilidade_geral_fem <- bind_rows(lapply(questoes, function(questao) {
  prop_table <- table(base1[base1$genero == "Feminino", ][[questao]])
  prop.table(prop_table) * 100
}), .id = "Questão")

acessibilidade_geral_fem$Questão <- c('Comunicacional','Atitudinal','Instrumental/Digital','Arquitetônica')
acessibilidade_geral_fem <- acessibilidade_geral_fem %>%  rename('Ótimo (%)' = Ótimo, 'Bom (%)' = Bom, 'Regular (%)' = Regular, 'Ruim (%)' = Ruim,'Péssimo (%)' = Péssimo)

acessibilidade_geral_masc <- bind_rows(lapply(questoes, function(questao) {
  prop_table <- table(base1[base1$genero == "Masculino", ][[questao]])
  prop.table(prop_table) * 100
}), .id = "Questão")


acessibilidade_geral_masc$Questão <- c('Comunicacional','Atitudinal','Instrumental/Digital','Arquitetônica')
acessibilidade_geral_masc <- acessibilidade_geral_masc %>%  rename('Ótimo (%)' = Ótimo, 'Bom (%)' = Bom, 'Regular (%)' = Regular, 'Ruim (%)' = Ruim,'Péssimo (%)' = Péssimo)

view(acessibilidade_geral_masc)
view(acessibilidade_geral_fem)





table(base1$campus,base1$como_considera_acessibilidade_arquitetonica_ufsm)
table(base1$campus)
prop.table(table(base1$campus,base1$como_considera_acessibilidade_arquitetonica_ufsm))*100

base_cachoeira <- base1[base1$campus == "Cachoeira do Sul", ]
base_cachoeira$como_considera_acessibilidade_arquitetonica_ufsm <- factor(base_cachoeira$como_considera_acessibilidade_arquitetonica_ufsm, levels = names(sort(table(base_cachoeira$como_considera_acessibilidade_arquitetonica_ufsm), decreasing = TRUE)))

base_frederico <- base1[base1$campus == "Frederico Westphalen", ]
base_frederico$como_considera_acessibilidade_arquitetonica_ufsm <- factor(base_frederico$como_considera_acessibilidade_arquitetonica_ufsm, levels = names(sort(table(base_frederico$como_considera_acessibilidade_arquitetonica_ufsm), decreasing = TRUE)))

base_palmeira <- base1[base1$campus == "Palmeira das Missões", ]
base_palmeira$como_considera_acessibilidade_arquitetonica_ufsm <- factor(base_palmeira$como_considera_acessibilidade_arquitetonica_ufsm, levels = names(sort(table(base_palmeira$como_considera_acessibilidade_arquitetonica_ufsm), decreasing = TRUE)))

base_santa <- base1[base1$campus == "Santa Maria", ]
base_santa$como_considera_acessibilidade_arquitetonica_ufsm <- factor(base_santa$como_considera_acessibilidade_arquitetonica_ufsm, levels = names(sort(table(base_santa$como_considera_acessibilidade_arquitetonica_ufsm), decreasing = TRUE)))

# Pra aparecer no grafico na ordem
ordem <- c("Ótimo", "Bom", "Regular", "Ruim", "Péssimo")

base_cachoeira$como_considera_acessibilidade_arquitetonica_ufsm <- factor(base_cachoeira$como_considera_acessibilidade_arquitetonica_ufsm, levels = ordem)

base_frederico$como_considera_acessibilidade_arquitetonica_ufsm <- factor(base_frederico$como_considera_acessibilidade_arquitetonica_ufsm, levels = ordem)

base_palmeira$como_considera_acessibilidade_arquitetonica_ufsm <- factor(base_palmeira$como_considera_acessibilidade_arquitetonica_ufsm, levels = ordem)

base_santa$como_considera_acessibilidade_arquitetonica_ufsm <- factor(base_santa$como_considera_acessibilidade_arquitetonica_ufsm, levels = ordem)

print(sprintf('Percentual de respostas bom ou otimo para campus cachoeira é de %.1f %%, para frederico é de %.1f %%, para campus palmeira é de %.1f %% e para santa é de %.1f %%,', 
        sum(base_cachoeira$como_considera_acessibilidade_arquitetonica_ufsm %in% c("Bom", "Ótimo")) / nrow(base_cachoeira) * 100, 
        sum(base_frederico$como_considera_acessibilidade_arquitetonica_ufsm %in% c("Bom", "Ótimo")) / nrow(base_frederico) * 100, 
        sum(base_palmeira$como_considera_acessibilidade_arquitetonica_ufsm %in% c("Bom", "Ótimo")) / nrow(base_palmeira) * 100,
        sum(base_santa$como_considera_acessibilidade_arquitetonica_ufsm %in% c("Bom", "Ótimo")) / nrow(base_santa) * 100))