#' @export
process_alterations <- function(alterations, include_hetdel = FALSE) {
    ##
    if (include_hetdel) hetdel_val = 0 else hetdel_val = Inf
    monos = with(alterations,
                 sign(hetdel > hetdel_val) + sign((som.mis + som.trunc) == 1)
                 )
    monos[monos == 0] = NA_real_
    monos[monos > 1] = NA_real_
    bla = as.data.table(melt(monos, na.rm = T))
    bla[, biallelic := FALSE]
    bla = setcols(bla, c("Var1", "Var2"), c("pair", "gene"))

    bis = with(
        alterations,
        sign(sign(loh > 0) + som.mis + som.trunc > 1)
    )
    bis[!bis > 0] = NA_real_
    bla2 = as.data.table(melt(bis, na.rm = T))
    bla2[, biallelic := TRUE]
    bla2 = setcols(bla2, c("Var1", "Var2"), c("pair", "gene"))

    amp.ccne1 = with(
        alterations,
        sign(amp > 0)
    )[, "CCNE1",drop=F]

    amps = melt(amp.ccne1 %>% replace2(x == 0, NA_real_), na.rm = T) %>% asdt %>%
        setcols(c("Var1", "Var2"), c("pair", "gene")) %>% mutate(value = NULL)

    mutlist = rbind(bla, bla2)[, type := "MUTATION"] %>% mutate(value = NULL)
    mutlist = rbind(mutlist, amps[, type := "AMP"], fill = T)
    
    return(mutlist)
}


#' @export
process_alt_for_hrd <- function(mutlist, gpath = "~/lab/projects/Starr/BRCA/files/hrd_gene_list_jsetton.csv") 
{
    hrdg = dt_empty2na(fread(gpath))
    if (!is.factor(mutlist$pair)) mutlist$pair = factor(mutlist$pair)
    init = data.table(pair = factor(levels(mutlist$pair), levels(mutlist$pair)))
    allg = lapply(hrdg, function(x) gsub("\\(HROB\\)", "", na.omit(x)) %>% 
        trimws) %>% do.call(what = c) %>% unname %>% unique
    allg = union(c("BRCA1", "BRCA2"), allg)
    hrd.genes = mutlist[(gene %in% c(allg, "CDK12") & type %nin% 
                         "AMP")]
    g.ccne1 = mutlist[gene %in% "CCNE1"][type %in% "AMP"]
    brca_bi = hrd.genes[(gene %in% c("BRCA1", "BRCA2") & biallelic == 
        TRUE)] %>% distinct(pair, gene, biallelic)
    brca_mono = hrd.genes[(gene %in% c("BRCA1", "BRCA2") & biallelic == 
        FALSE)] %>% distinct(pair, gene, biallelic)
    others_bi = hrd.genes[(!gene %in% c("BRCA1", "BRCA2")) & 
        biallelic == TRUE] %>% distinct(pair, gene, biallelic)
    others_mono = hrd.genes[(!gene %in% c("BRCA1", "BRCA2")) & 
        biallelic == FALSE] %>% distinct(pair, gene, biallelic)
    bi_length = function(x) sign(length(x))
    bi_brca = brca_bi %>% mutate(gene = fct_drop(gene) %>% factor(levels = c("BRCA1", 
        "BRCA2"))) %>% dcast.data.table(pair ~ gene, fun.aggregate = bi_length, 
        drop = F) %>% rename_all(tolower) %>% rename_at(vars(-1), 
        ~paste0("bi_", .))
    mono_brca = brca_mono %>% mutate(gene = fct_drop(gene) %>% 
        factor(levels = c("BRCA1", "BRCA2"))) %>% dcast.data.table(pair ~ 
        gene, fun.aggregate = bi_length, drop = F) %>% rename_all(tolower) %>% 
        rename_at(vars(-1), ~paste0("mono_", .))
    other_bi = (others_bi %>% mutate(gene = fct_drop(gene)))[, 
        .(other_bi = paste0(gene, collapse = " ")), by = pair] %>% 
        complete(pair) %>% asdt
    other_mono = (others_mono %>% mutate(gene = fct_drop(gene)))[, 
        .(other_mono = paste0(gene, collapse = " ")), by = pair] %>% 
        complete(pair) %>% asdt
    amp_ccne1 = (g.ccne1 %>% mutate(gene = fct_drop(gene)))[, 
        .(amp_ccne1 = 1), by = pair] %>% complete(pair) %>% asdt
    mutstat = rbind(hrd.genes, g.ccne1, fill = T)[, .(mutstat = paste0(unique(gene), 
        collapse = " ")), by = pair] %>% complete(pair) %>% asdt %>% 
        mutate(mutstat = replace_na(mutstat, "WT"))
    fintbl = Reduce(function(x, y) if (NROW(x) && NROW(y)) merge(x, y, by = "pair") else if (NROW(x)) x else if (NROW(y)) y, 
        list(bi_brca, mono_brca, other_bi, other_mono, amp_ccne1, 
            mutstat), init = init)
    return(fintbl)
}
