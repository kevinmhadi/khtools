#' @export
process_alterations <- function(alterations, include_hetdel = FALSE, 
                                # include_homdel = FALSE, 
                                na.fun = khtools::na2zero) {
    if (all(is.na(na.fun)) || is.null(na.fun)) {
        na.fun = identity
    } else if (is.character(na.fun)) {
        nafun = getFunction(na.fun)
    } else if (is.function(na.fun)) {
        nafun = na.fun
    } else {
        nafun = identity
    }
    if (include_hetdel) hetdel_val = 0 else hetdel_val = Inf
    ## monos = with(alterations,
    ##              sign(hetdel > hetdel_val) + sign((som.mis + som.trunc) == 1)
    ##              )
    monos = with(
        alterations, {
            sign(
                sign(nafun(hetdel) > hetdel_val) +
                nafun(som.mis) +
                nafun(som.trunc) +
                nafun(germ.patho) +
                nafun(germ.trunc) == 1
            )
        }
    )
    
    # hetdels = with(
    #     alterations, {
    #         sign(
    #             sign(nafun(hetdel) > 0)
    #         )
    #     }
    # )
    # globasn(hetdels) ## for debugging
    
    monos[monos == 0] = NA_real_
    monos[monos > 1] = NA_real_
    bla = as.data.table(melt(monos, na.rm = T))
    bla[, biallelic := FALSE]
    bla = setcols(bla, c("Var1", "Var2"), c("pair", "gene"))

    bis = with(
        alterations,
        sign(
            (sign(nafun(loh) > 0) +
            ## sign(nafun(hetdel) > 0) +
            nafun(som.mis) +
            nafun(som.trunc) +
            nafun(germ.patho) +
            nafun(germ.trunc) > 1) +
            sign(nafun(homdel > 0))
        )
    )
    bis[!bis > 0] = NA_real_
    bla2 = as.data.table(melt(bis, na.rm = T))
    bla2[, biallelic := TRUE]
    bla2 = setcols(bla2, c("Var1", "Var2"), c("pair", "gene"))
    
    # if (isTRUE(include_homdel)) {
    #     homdels = with(
    #         alterations,
    #         sign(
    #             nafun(homdel) > 0
    #         )
    #     )
    #     homdels[!homdels > 0] = NA_real_
    #     mhomdels = as.data.table(melt(homdels, na.rm = T))
    #     mhomdels[, biallelic := TRUE]
    #     mhomdels = setcols(mhomdels, c("Var1", "Var2"), c("pair", "gene"))
    # }
        


    amp.ccne1 = with(
        alterations,
        sign(amp > 0)
    )[, "CCNE1",drop=F]

    amps = melt(amp.ccne1 %>% replace2(x == 0, NA_real_), na.rm = T) %>% asdt %>%
        setcols(c("Var1", "Var2"), c("pair", "gene")) %>% mutate(value = NULL)

    mutlist = rbind(bla, bla2)[, type := "MUTATION"] %>% mutate(value = NULL)
    mutlist = rbind(mutlist, amps[, type := "AMP"], fill = T)
    
#     if (isTRUE(include_homdel))
#         mutlist = rbind(mutlist, mhomdels[, type := "HOMDEL"], fill = T)
    
    return(mutlist)
}

process_hetdels <- function(alterations, na.fun = khtools::na2zero) {
    if (all(is.na(na.fun)) || is.null(na.fun)) {
        na.fun = identity
    } else if (is.character(na.fun)) {
        nafun = getFunction(na.fun)
    } else if (is.function(na.fun)) {
        nafun = na.fun
    } else {
        nafun = identity
    }
    ## monos = with(alterations,
    ##              sign(hetdel > hetdel_val) + sign((som.mis + som.trunc) == 1)
    ##              )
    hetdels = with(
        alterations, {
                sign(nafun(hetdel) > 0)
        }
    )
    hetdels[!hetdels > 0] = NA_real_
    mhetdels = as.data.table(melt(hetdels, na.rm = T))
    mhetdels[, biallelic := FALSE]
    mhetdels = setcols(mhetdels, c("Var1", "Var2"), c("pair", "gene"))
    mhetdels[, type := "HETDEL"]
    
    return(mhetdels)
    
}

patch_hetdels = function(hetdels) {
    het_length = function(x) sign(length(x))
init = data.table(pair = factor(levels(hetdels$pair), levels(hetdels$pair)))

brca_hetdel = hetdels[gene %in% c("BRCA1", "BRCA2")] %>% distinct(pair, gene, biallelic) %>% mutate(gene = fct_drop(gene) %>% factor(levels = c("BRCA1", 
        "BRCA2"))) %>% dcast.data.table(pair ~ gene, fun.aggregate = het_length, 
        drop = F) %>% rename_all(tolower) %>% rename_at(vars(-1), 
        ~paste0("hetdel_", .))
brca_hetdel = Reduce(function(x, y) if (NROW(x) && NROW(y)) 
    merge(x, y, by = "pair")
else if (NROW(x)) 
    x
else if (NROW(y)) 
    y, list(brca_hetdel), init = init)
}

process_homdels <- function(alterations, na.fun = khtools::na2zero) {
    if (all(is.na(na.fun)) || is.null(na.fun)) {
        na.fun = identity
    } else if (is.character(na.fun)) {
        nafun = getFunction(na.fun)
    } else if (is.function(na.fun)) {
        nafun = na.fun
    } else {
        nafun = identity
    }
    ## monos = with(alterations,
    ##              sign(hetdel > hetdel_val) + sign((som.mis + som.trunc) == 1)
    ##              )
    homdels = with(
        alterations, {
                sign(nafun(homdel) > 0)
        }
    )
    homdels[!homdels > 0] = NA_real_
    mhomdels = as.data.table(melt(homdels, na.rm = T))
    mhomdels[, biallelic := TRUE]
    mhomdels = setcols(mhomdels, c("Var1", "Var2"), c("pair", "gene"))
    mhomdels[, type := "HOMDEL"]
    
    return(mhomdels)
    
}

patch_homdels = function(homdels) {
    het_length = function(x) sign(length(x))
init = data.table(pair = factor(levels(homdels$pair), levels(homdels$pair)))

brca_homdel = homdels[gene %in% c("BRCA1", "BRCA2")] %>% distinct(pair, gene, biallelic) %>% mutate(gene = fct_drop(gene) %>% factor(levels = c("BRCA1", 
        "BRCA2"))) %>% dcast.data.table(pair ~ gene, fun.aggregate = het_length, 
        drop = F) %>% rename_all(tolower) %>% rename_at(vars(-1), 
        ~paste0("homdel_", .))
brca_hetdel = Reduce(function(x, y) if (NROW(x) && NROW(y)) 
    merge(x, y, by = "pair")
else if (NROW(x)) 
    x
else if (NROW(y)) 
    y, list(brca_homdel), init = init)
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
