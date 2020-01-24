


#' match x indices in terms of y
#'
#' @param x A vector
#' @param y A vector
#' @return a vector of indices of \code{x} ordered by \code{y}
#' @examples
#' match_s(c(1,3,5,7,9), c(9, 5, 3))
#' match_s(c(1,3,5,7,9), c(3, 5, 9))
#' @export
match_s = function(x, y) {
    ## x_tmp = factor(as.character(x), levels = as.character(y))
    ## y_tmp = factor(as.character(y), levels = as.character(x))
    ## y_tmp[which(y_tmp %in% x_tmp)]
    x_tmp = setNames(as.character(x), as.character(x))
    x_ind = setNames(1:length(x), as.character(x))
    y_tmp = setNames(as.character(y), as.character(y))
    y_ind = setNames(1:length(y), as.character(y))
    ## return(x_ind[names(y_tmp)[which(y_tmp %in% x_tmp)]])
    these_idx = which(y_tmp %in% x_tmp)
    find_in_x = names(y_tmp)[these_idx]
    names(find_in_x) = y_ind[these_idx]
    return(setNames(x_ind[find_in_x], names(find_in_x)))
}

#' matches x in terms of y
#'
#' returns vector of indices of matches in x with length of vector = length(y)
#' non matches are NA
#' @export
match2 = function(x, y) {
    ## x_tmp = factor(as.character(x), levels = as.character(y))
    ## y_tmp = factor(as.character(y), levels = as.character(x))
    ## y_tmp[which(y_tmp %in% x_tmp)]
    x_tmp = setNames(as.character(x), as.character(x))
    x_ind = setNames(1:length(x), as.character(x))
    y_tmp = setNames(as.character(y), as.character(y))
    y_ind = setNames(1:length(y), as.character(y))
    ## return(x_ind[names(y_tmp)[which(y_tmp %in% x_tmp)]])
    these_idx = which(y_tmp %in% x_tmp)
    find_in_x = names(y_tmp)[these_idx]
    names(find_in_x) = y_ind[these_idx]
    new_index = rep(NA, length(y_tmp))
    new_index[y_ind[these_idx]] = x_ind[find_in_x]
    return(new_index)
}

#' find all duplicates in a vector
#'
#' @param vec A vector
#' @return a logical vector with all positions marked TRUE being duplicates
#' @examples
#' find_dups(c(1,1,1,3,5))
#' find_dups(c(1,3,1,3,1))
#' find_dups(c(3,1,5,4,4))
#' @export
find_dups = function(vec, re_sort = FALSE) {
    dups = unique(vec[ duplicated(vec)])
    if (!re_sort) {
        return(vec %in% dups)
    } else {
        matching_idx = match2(sort(dups), vec)
        return(which(!is.na(matching_idx))[order(na.omit(matching_idx))])
    }
}

#' @name undup
#' @title an alternative to base::unique() that preserves names
#'
#' @param obj an R vector
#' @return unique values of obj with names preserved
#' @export
undup = function(obj, fromLast = FALSE, nmax = NA) {
    obj[!duplicated(obj, fromLast = fromLast, nmax = NA)]
}



#' name a character vector to itself 
#'
#' @title selfname
#' @param char A character vector
#' @return A named character vector
#' @export
selfname = function(char) {setNames(char, char)}

#' @title check_lst
#' checking a list for any elements that are try-errors
#' usually from an lapply(..., function(x) try({})) call
#'
#' @param lst A list
#' @return a logical vector marking which elements are try-errors"
#' @export
check_lst = function(lst, class_condition = c("try-error", "error", "errored", "err"))
{
    unlist(lapply(lst, function(x) class(x)[1])) %in% class_condition
}

#' a wrapper around check_lst
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return only returns the non-errors in the list
#' @export
ret_no_err = function(lst, class_condition = c("try-error", "error", "errored", "err"))
{
    return(lst[!check_lst(lst, class_condition = class_condition)])
}

#' a wrapper around check_lst
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return only returns the errors in the list
#' @export
ret_err = function(lst, class_condition = c("try-error", "error", "errored", "err"))
{
    return(lst[check_lst(lst, class_condition = class_condition)])
}

#' using check_lst to return
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return returns full length list with errored elements changed to NA
#' @export
ret_na_err = function(lst, class_condition = c("try-error", "error", "errored", "err"))
{
    lst[check_lst(lst, class_condition = class_condition)] = NA
    return(lst)
}


#' convenience function to set column names
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return colnamed object
#' @export
setColnames = function(object = nm, nm = NULL, pattern = NULL, replacement = "") {
    if (!is.null(nm)) {
        colnames(object)  = nm
    } else if (!is.null(pattern)) {
        colnames(object) = gsub(pattern, replacement, colnames(object))
    }
    return(object)
}


#' convenience function to set row names
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return rownamed object
#' @export
setRownames = function(object = nm, nm) {
    base::rownames(object) = nm
    object
}


#' collate two vectors together
#'
#' @param ... A set of vectors to collate
#' @return a vector with values of inputs collated together
#' @examples
#' intercalate(c("a","d","f"), c("b", "e", "g", "z"))
#' @export
intercalate = function(...) {
    args = list(...)
    if (isNested(args)) {
        args = unlist(args, recursive = F)
    }
    s_along = lapply(args, seq_along)
    ord = order(do.call(c, s_along))
    conc = do.call(c, args)
    return(conc[ord])
}


#' convenience function to convert to matrix
#' and optionally filter out the first column
#' which may be rownames that are not relevant to further data analysis
#'
#' @param obj a data.frame or matrix
#' @param rm_col1 a logical vector specifying if the 1st column should be removed
#' @return a matrix
#' @export
matrify = function(obj, rm_col1 = TRUE, use.c1.rownames = TRUE) {
    if (rm_col1) {
        if (use.c1.rownames) {
            rn = as.matrix(obj[,1])[,1, drop = TRUE]
        } else {
            rn = NULL
        }
        setRownames(as.matrix(obj[,-1]), rn)
    } else {
        as.matrix(obj)
    }
}


#' collate lists together
#'
#' @param ... A set of lists to collate
#' @return a lists with elements collated together
#' @examples
#' intercalate(list(paste0(1:5, "_A")), list(paste0(1:3, "_B")), list(paste0(1:6, "_C")))
#' @export
intercalate_lst = function(...) {
    args = list(...)
    s_along = lapply(args, seq_along)
    ord = order(do.call(c, s_along))
    conc = do.call(c, args)
    return(conc[ord])
}


#' @name ix_sdiff
#'
#' A function that subsets out indices and is robust to
#' if filt_out indices are integer(0)
#'
#' @return obj with indices indicated in filt_out taken out
#' @export
ix_sdiff = function(obj, filt_out) {
    if (is.null(nrow(obj))) {
        ix = 1:length(obj)
    } else {
        ix = 1:nrow(obj)
    }
    obj[! ix %in% filt_out]
}

#' @name na2false
#'
#' A convenience function to set a logical vector with NAs to false
#'
#' @return A logical vector with NAs set to FALSE
#' @export
na2false = function(v)
{
    ## v = ifelse(is.na(v), v, FALSE)
    v[is.na(v)] = FALSE
    as.logical(v)
}


#' @name na2true
#'
#' A convenience function to set a logical vector with NAs to TRUE
#'
#' @return A logical vector with NAs set to TRUE
#' @export
na2true = function(v)
{
    ## v = ifelse(is.na(v), v, FALSE)
    v[is.na(v)] = TRUE
    as.logical(v)
}

#' @name na2zero
#'
#' A convenience function to set a numeric vector with NAs to zero
#'
#' @return A numeric vector with NAs set to zero
#' @export
na2zero = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    v[is.na(v)] = 0
    return(v)
}

#' @name nan2zero
#'
#' A convenience function to set a numeric vector with NaNs to zero
#'
#' @return A numeric vector with NaNs set to zero
#' @export
nan2zero = function(v) {
    v[is.nan(v)] = 0
    return(v)
}


#' @name na2empty
#'
#' A convenience function to set a character vector with NAs to an
#' empty character
#'
#' @return A character vector
#' @export
na2empty = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    v[is.na(v)] = ""
    as.character(v)
}


#' @name empty2na
#'
#' A convenience function to set a character vector with NAs to an
#' empty character
#'
#' @return A character vector
#' @export
empty2na = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    v[nchar(v) == 0] = as.character(NA)
    v
}


#' @name ws2und
#'
#' make data.frame or data.table column name whitespaces into
#' underscores and remove end whitespaces
#'
#' @return A character vector
#' @export
ws2und = function(df)
{
    data.table::setnames(df, gsub("^_|_$", "", gsub("_{2,}", "_", gsub("(\\/)|(\\.)|( )|\\(|\\)|\\#", "_", trimws(colnames(df))))))
    return(df)
}


#' @name lst.empty
#'
#' A logical vector to select which list elements are empty
#'
#' @return A list
#' @export
lst.empty = function(x) {
    S4Vectors::elementNROWS(x) == 0
}

#' @name lst.empty2zero
#'
#' set empty list elements to zero
#'
#' @return A logical vector of length(x)
#' @export
lst.empty2zero = function(x) {
    x[S4Vectors::elementNROWS(x) == 0] = 0
}


#' @name lst.empty2na
#'
#' set empty list elements to NA
#'
#' @return A list
#' @export
lst.empty2na = function(x) {
    x[S4Vectors::elementNROWS(x) == 0] = NA
    ## x[x == "character(0)"] = NA
    ## x[x == "numeric(0)"] = NA
    ## x[x == "logical(0)"] = NA
    ## x[x == "integer(0)"] = NA
    x
}

#' @name lst.empty2null
#'
#' set empty list elements to NULL
#'
#' @return A list
#' @export
lst.empty2null = function(x) {
    x[S4Vectors::elementNROWS(x) == 0] = NULL
    ## x[x == "character(0)"] = NULL
    ## x[x == "numeric(0)"] = NULL
    ## x[x == "logical(0)"] = NULL
    ## x[x == "integer(0)"] = NULL
    x
}


#' @name lst.null2na
#'
#' set NULL list elements to NA
#'
#' @return A list
#' @export
lst.null2na = function(x) {
    x[x == "NULL"] = NA
    x
}

#' @name lst.emptychar2null
#'
#' set empty character to null
#'
#' @return A list
#' @export
lst.emptychar2null = function(x) {
    x[x == ""] = list(NULL)
}

#' @name lst.zerochar2empty
#'
#' set 0 length chracter to empty
#'
#' @return A list
#' @export
lst.zerochar2empty = function(x) {
    x[x == "character(0)"] = list("")
}


################################################## general R utilities
##################################################
##################################################



#' @name good.file
#'
#' Does file exist and is its size greater than a threshold
#'
#' @return logical
#' @export
good.file = function(x, size.thresh = 0) {
    (file.exists(x) & na2false(file.size(x) > size.thresh))
}


#' @export
loop_grep = function(pattern, x, ignore.case = FALSE) {
    ## for (i in unique(pattern)) {
    ## matches = integer(0)
    ## for (i in seq_along(pattern)) {
    ##     this_id = grep(pattern = pattern[i], x)
    ##     matches = unique(c(matches, this_id))
    ## }
    pattern = unique(pattern)
    ind = unlist(lapply(pattern, function(this_pattern) {
        grep(pattern = this_pattern, x, ignore.case = ignore.case)
    }))
    return(ind)
}

#' @export
loop_grepl = function(patterns, vec_char, ignore.case = FALSE) {
    lg = logical(length(vec_char))
    ind = loop_grep(patterns, vec_char, ignore.case = ignore.case)
    lg[ind] = TRUE
    lg
}




#' @name rrrepeated
#'
#' Recursively repeat a function
#' Found on stackoverflow
#' 
#' @return Same as .x
#' @export
rrrepeated <- function(.x, .reps = 1, .f, ...) {
    library(purrr)
                                        # A single, finite, non-negative number of repetitions
    assertthat::assert_that(
        length(.reps) == 1,
        !is.na(.reps),
        .reps >= 0,
        is.finite(.reps))

                                        # accept purrr-style formula functions
    .f <- rlang::as_function(.f, ...)

    recursively_repeat <- function(.x, .reps, .f, ...) {
        if (.reps == 0) {
            .x
        } else {
            ## recursively_repeat(.f(.x, ...), .reps - 1, .f, ...)
            Recall(.f(.x, ...), .reps - 1, .f, ...)
                                        # (It would be more correct to use `Recall()` so that renaming the function
                                        # doesn't break this line... -- how's that for an R deep cut?)
        }
    }

    recursively_repeat(.x, .reps, .f, ...)
}


#' @name ez_string
#'
#' An easy way to print out a c() vector into a blog file
#'
#' @export
ez_string = function(string_vec, c = T, list = !c, quotes = T, ws = "\n") {
    ws = paste0(",", ws)
    op_string_c = "c("
    op_string_l = "list("
    if (quotes) {
        q = "\""
    }
    else
        q = NULL
    c_cmd = expression(cat(paste0(op_string_c, paste0(q, string_vec, q, collapse = ws), ")\n")))
    list_cmd = expression(cat(paste0(op_string_l, paste0(q, string_vec, q, collapse = ws), ")\n")))
    lst_args = as.list(match.call())
    c_arg = eval(lst_args$c)
    l_arg = eval(lst_args$list)
    c_arg_cond = tryCatch(! is.null(c_arg) & c_arg, error = function(e) FALSE)
    l_arg_cond = tryCatch(! is.null(l_arg) & l_arg, error = function(e) FALSE)
    if (all(is.null(c(c_arg, l_arg))))
        eval(c_cmd)
    else if(c_arg_cond & l_arg_cond) {
        eval(c_cmd)
        eval(list_cmd)
    }
    else if (l_arg_cond)
        eval(list_cmd)
    else if (is.null(c_arg) & ! l_arg_cond)
        eval(c_cmd)
    else if (! c_arg_cond)
        eval(list_cmd)
    else
        eval(c_cmd)
}


#' @name try2
#'
#' A slightly more robust version of try that works within the parallel:: set of functions
#' that pre-deploy a cluster.
#'
#' @export
try2 = function(expr, ..., finally) {
    tryCatch(expr,
             error = function(e) {
                 msg = structure(paste(conditionMessage(e), conditionCall(e), sep = "\n"), class = "err")
                 cat("Error: ", msg, "\n\n")
                 return(msg)
             },
             finally = finally,
             ... = ...)
}


#' @name dedup.cols
#'
#' dedup the column names of a data.frame/data.table
#'
#' @return A data.table or data.frame
#' @export
dedup.cols = function(tbl) {
    if (!inherits(tbl, "data.table"))
        tbl[, match(unique(colnames(tbl)), colnames(tbl))]
    else
        tbl[, match(unique(colnames(tbl)), colnames(tbl)), with = FALSE]
}


#' @name pinch.frac
#'
#' A convenience function to transform proportions.
#' Useful for beta regression (library(betareg))
#'
#' @return A vector
#' @export
pinch.frac = function(x, fmin = 0.01, fmax = 0.99) {
    pmax(pmin(x, fmax), fmin)
}

#' @name binom.conf
#'
#' A convenience function to get confidence intervals around
#' proportions. Useful for beta regression (library(betareg)).
#'
#' @return A vector
#' @export
binom.conf = function(n, tot, alpha = 0.025) {
    conf.low = qbinom(p = (1 - (alpha)), size = tot, prob = n / tot, lower.tail = FALSE) / tot
    conf.high= qbinom(p = (1 - (alpha)), size = tot, prob = n / tot, lower.tail = TRUE) / tot
    data.table(frac = n / tot, conf.low, conf.high)
}


#' @name getdat
#'
#' to be used within "with()" within the expression
#'
#' @return data.frame/data.table
#' @export
getdat = function() { ## to be used within "with()" expr
    pf = parent.frame(3)
    if (identical(environmentName(pf), "R_GlobalEnv"))
        return(invisible(NULL))
    if ("data" %in% names(pf))
        data = get("data", pf)
    else
        data = get("envir", pf)
    if (is.environment(data))
        data = get("data", data)
        ## data = data$data
    data
    ## with(, {
    ##     data = get("data", parent.frame(2))
    ## })
}

#' @name gd
#' 
#' @export
gd = getdat


#' @name withv
#'
#' to be used for quick interactive programming
#' withv(toolongtotypemeagain, x * sum(x))
#'
#' @export
withv = function(x, expr) {
    eval(substitute(expr), enclos = parent.frame())
}



with2 = function(data, expr, ...) {
    data = data
    eval(substitute(expr), data)
}

#' @name file.info2
#'
#' A more robust file.info2 that removes any paths that do not exist
#'
#' @return data.frame/data.table
#' @export
file.info2 = function(fn, col = NULL, include.all = FALSE) {
    if (is.null(col)) col = as.character(substitute(fn))
    fif = file.info(unique(subset2(fn, file.exists(x)))) %>% rownames_to_column(col) %>% as.data.table
    if (include.all) {
        fif = merge(setnames(data.table(fn), col)[, tmp.ord := seq_along(fn)],
                    fif,
                    by = col, all = TRUE)[order(tmp.ord)][, tmp.ord := NULL]
    }
    fif
}


#' @name subset2
#'
#' convenience function to subset without having to type excessively
#' if the variable is arrived at through nested functions or long
#' variable names
#'
#' @export
subset2 = function(x, sub.expr, ...) {
    if (!missing(sub.expr))
        this.sub = eval(as.list(match.call())$sub.expr)
    else if (missing(sub.expr)) {
        if (!is.null(dim(x)))
            this.sub = seq_len(nrow(x))
        else
            this.sub = seq_along(x)
    }
    subset(x, this.sub, ...)
}



#' @name replace2
#'
#' convenience function to replace without having to type excessively
#' if the variable is arrived at through nested functions or long
#' variable names
#'
#' @export
replace2 = function(x, repl.expr, values) {
    this.repl = eval(as.list(match.call())$repl.expr)
    replace(x, this.repl, values = values)
}



#' @export
ave2 = function(x, ..., FUN = mean) {
    if (missing(...)) 
        x[] <- FUN(x)
    else {
        g <- interaction(...)
        x = lapply(split(x, g), FUN)
    }
    x
}

#' @name rematch
#'
#' reconstruct the original vector from a vmatch
#'
#' @export
rematch = function (vmatch_out)  {
    this = vmatch_out$matches
    this[is.na(this)] = na.omit(vmatch_out$unmatch)
    this
}

#' @name vmatch
#'
#' get match indices and non matched indices
#'
#' @export
vmatch = function(x, y, ...) {
    m = match(x, y, ...)
    unm = rep(NA, length(x))
    unm[is.na(m)] = x[is.na(m)]
    list(matches = y[m], unmatch = unm)
}

#' @name file.mat.exists
#'
#' @export
file.mat.exists = function(x) {
    matrify(x) %>% {setRownames(apply(., 2, file.exists), rownames(.))}
}


#' @name `%nin%`
#'
#' Not match
#' 
#' @export
`%nin%` = function (x, table) 
{
    match(x, table, nomatch = 0L) == 0L
}

#' @name f2int
#'
#' robustly convert a factor to integer
#'
#' @return factor
#' @export
f2int = function(this_factor) {
    if (inherits(this_factor, "factor")) {
        lvl = levels(this_factor)
        if (inherits(lvl, c("numeric", "integer"))) {
            as.integer(levels(this_factor))[this_factor]   
        } else if (inherits(lvl, "character")) {
            match(as.character(this_factor), lvl)
        }
    } else {
        warning("Did not supply a factor, returning object as is")
        this_factor
    }
}

#' @name system3
#'
#' A modification of system2 to be able to return either or the stderr
#' or stdout to console or flexibly return to a path.
#'
#' @export
system3 = function (command, args = character(), stdout = "", stderr = "", 
    stdin = "", input = NULL, env = character(), wait = TRUE, 
    minimized = FALSE, invisible = TRUE, timeout = 0) 
{
    if (!missing(minimized) || !missing(invisible)) 
        message("arguments 'minimized' and 'invisible' are for Windows only")
    if (!is.logical(wait) || is.na(wait)) 
        stop("'wait' must be TRUE or FALSE")
    intern <- FALSE
    command <- paste(c(env, shQuote(command), args), collapse = " ")
    if (is.null(stdout)) 
        stdout <- FALSE
    if (is.null(stderr)) 
        stderr <- FALSE
    if (isTRUE(stdout) || isTRUE(stderr))
        intern <- TRUE
    if (as.integer((isTRUE(stdout) | isFALSE(stdout)) + (isTRUE(stderr) | isFALSE(stderr))) == 2) {
        if (isTRUE(stderr) | isTRUE(stdout)) intern = TRUE
        if (isTRUE(stderr) & isFALSE(stdout)) {
            command = paste(command, "2>&1", ">/dev/null")
            ## command <- paste(command, "2>/dev/null")
        } else if (isTRUE(stderr) & isTRUE(stdout)) {
            command = paste(command, "2>&1")
        } else if (isFALSE(stderr) & isTRUE(stdout)) {
            command = paste(command, "2>/dev/null")
        }
    } else if (isTRUE(stderr) & is.character(stdout)) {
        if (length(stdout) != 1L) {
            stop("'stdout' must be of length 1")
        }
        if (nzchar(stdout)) {
            command = paste(command, "2>&1", ">", shQuote(stdout))
        } else {
            command = paste(command, "2>&1", ">/dev/null")
        }
    } else if (is.character(stderr) & isTRUE(stdout)) {
        if (length(stderr) != 1L) {
            stop("'stderr' must be of length 1")
        }
        if (nzchar(stderr)) {
            command = paste(command, "2>", shQuote(stderr))
        }
    } else if (is.character(stderr) & is.character(stdout)) {
        if (length(stdout) != 1L) 
            stop("'stdout' must be of length 1")
        if (nzchar(stdout)) {
            command <- if (identical(stdout, stderr)) 
                           paste(command, ">", shQuote(stdout), "2>&1")
                       else paste(command, ">", shQuote(stdout))
        }
        if (length(stderr) != 1L) 
            stop("'stderr' must be of length 1")
        if (nzchar(stderr) && !identical(stdout, stderr)) 
            command <- paste(command, "2>", shQuote(stderr))
    }
    if (!is.null(input)) {
        if (!is.character(input)) 
            stop("'input' must be a character vector or 'NULL'")
        f <- tempfile()
        on.exit(unlink(f))
        writeLines(input, f)
        command <- paste(command, "<", shQuote(f))
    }
    else if (nzchar(stdin)) 
        command <- paste(command, "<", stdin)
    if (!wait && !intern) 
        command <- paste(command, "&")
    .Internal(system(command, intern, timeout))
}

is.empty = function(x) {
    if (!is.null(dim(x))) {
        dim(x)[1] == 0
    } else {
        length(x) == 0 || is.null(x)
    }
}

#' @name min.col.narm
#'
#' Return the index of the minimum column per row while removing NA
#'
#' @export min.col.narm
min.col.narm = function(mat, ties.method = "first") {
    ok = max.col(-replace(mat, is.na(mat), Inf), ties.method=ties.method) * NA ^ !rowSums(!is.na(mat))
    return(ok)
}

#' @name min.col.narm
#'
#' Return the index of the maximum column per row while removing NA
#'
#' @export max.col.narm
max.col.narm = function(mat, ties.method = "first") {
    ok = max.col(replace(mat, is.na(mat), -Inf), ties.method=ties.method) * NA ^ !rowSums(!is.na(mat))
    return(ok)
}


#' @name table2
#'
#' Convenience wrapper around table to show NA if there are any
#'
#' @export
table2 = function(...) {
    return(table(..., useNA = "ifany"))
}

#' @name table3
#'
#' Convenience function to always show NA counts
#'
#' @export
table3 = function(...) {
    return(table(..., useNA = "always"))
}

#' @name dig_dir
#'
#' Convenience wrapper around dir() to pull out files from the same
#' directory of a given file.
#'
#' @export
dig_dir = function(x, pattern = NULL, full.names = TRUE, mc.cores = 1, unlist = TRUE) {
    ## unlist(lst.empty2na(mclapply(dirname(x), function(y) {
    ##     dir(y, pattern = pattern, full.names = T)
    ## }, mc.cores = mc.cores)))
    if (unlist == TRUE) {
        unlist(lst.empty2na(mcMap(function(m.x, m.pattern) {
            dir(path = m.x, pattern = m.pattern, full.names = full.names)
        }, dirname(x), pattern, mc.cores = mc.cores)))
    } else {
        lst.empty2na(mcMap(function(m.x, m.pattern) {
            dir(path = m.x, pattern = m.pattern, full.names = full.names)
        }, dirname(x), pattern, mc.cores = mc.cores))
    }
}



#' @name stack.dt
#'
#' Collapse a named list with vectors as each element into a data.table
#'
#' @export
stack.dt = function(lst, ind = "ind", values = "values", ind.as.character = TRUE) {
    if (!length(lst) == 0) {
        dt = setDT(stack(lst))
        if (ind.as.character) {
            dt[, ind := as.character(ind)]
        }
    } else {
        dt = data.table(ind = character(0), values = numeric(0))
    }
    data.table::setnames(dt, c("ind", "values"), c(ind, values))
}

#' @name make_chunks
#'
#' Create chunks from a vector with a certain number of elements per chunk
#'
#' @return A list
#' @export
make_chunks = function(vec, num_per_chunk = 100) {
    ind = parallel::splitIndices(length(case_id), max(length(case_id) / max_per_chunk))
    split(case_id, rep(seq_along(ind), times = elementNROWS(ind)))
}


#' assign an object to global environment
#'
#' ONLY USE IF YOU KNOW WHAT YOU ARE DOING
#' This function forces assignment of a variable/function
#' to the global environment
#'
#' @param obj The object to assign to the global environment
#' @param var Optional name of variable, specified as string
#' @return either NULL or the object being assigned
#' @export
globasn = function(obj, var = NULL, return_obj = TRUE, envir = .GlobalEnv, verbose = TRUE)
{
    if (is.null(var)) {
        globx = as.character(substitute(obj))
    } else {
        if (!is.character(var)) {
            stop("var must be specified as character")
        }
        if (inherits(var, "character")) {
            ## if (var != as.character(substitute(var))) {
            ##     message("variable being assigned to ", var)
            ## }
            globx = var
        } else {
            globx = as.character(substitute(var))
            ## message("variable being assigned to ", globx)
        }
    }
    if (verbose)
        message("variable being assigned to ", globx)
    assign(globx, value = obj, envir = envir)
    if (return_obj) {
        invisible(obj)
    } else {
        NULL
    }
}



##############################
############################## factor helpers / forcats wrappers
##############################

#' @name refactor
#'
#' Keep one level of a factor and set all others to a specified level
#'
#' @return A factor
#' @export
refactor = function(fac, keep, ref_level = "OTHER") {
    if (!inherits(fac, "factor")) {
        fac = factor(fac)
    }
    new_fac = fct_explicit_na(factor(fac, levels = intersect(levels(fac), keep), ordered = FALSE), na_level = ref_level) %>%
        relevel(ref_level)
    new_fac
}



################################################## Flow utilities
##################################################
##################################################
##################################################
##################################################
#################################################


#' @name idj
#'
#' Match up ids to a job
#'
#' @return A Flow job object
#' @export
idj = function(x, these.ids) {
    x[match(these.ids, ids(x))]
}

#' @name reset.job
#'
#' Reset a job with different params
#'
#' @return A Flow job object
#' @export
reset.job = function(x, ..., jb.mem = x@runinfo$mem, jb.cores = x@runinfo$cores, update_cores = 1) {
    args = list(...)
    new.ent = copy(entities(x))
    for (i in seq_along(args))
    {
        if (!names(args)[i] %in% names(new.ent)) stop("adding additional column to entities... this function is just for resetting with new arguments")
        data.table::set(new.ent, j = names(args)[i], value = args[[i]])
    }
    Job(x@task, new.ent, rootdir = x@rootdir, mem = jb.mem, cores = jb.cores, update_cores = update_cores)
}

#' @name getcache
#'
#' Get the path of a Flow job object's cache.
#'
#' @return A character
#' @export
getcache = function(object) {
      path = paste(object@rootdir, "/", task(object)@name, 
                   ".rds", sep = "")
      return(path)
}


############################## GLM Utilities
##############################
##############################
##############################
#' @name glm.nb2
#'
#' Run a negative binomial regression.
#' If it fails, run a poisson regression
#'
#' @return A GLM model
#' @export
glm.nb2 = function(...) {
    mod = tryCatch(glm.nb(...), error = function(e) {
        warning("glm.nb broke... using poisson")
        return(glm(..., family = "poisson"))
    })
    ## mod = tryCatch(glm.nb(...), error = function(e) as.character(e))
    ## if (is.character(mod) &&
    ##     mod %in% c("Error in while ((it <- it + 1) < limit && abs(del) > eps) {: missing value where TRUE/FALSE needed\n",
    ##                "Error in glm.fitter(x = X, y = Y, w = w, etastart = eta, offset = offset, : NA/NaN/Inf in 'x'\n",
    ##                "Error: no valid set of coefficients has been found: please supply starting values\n")){
    ##     warning("theta parameter approaching infinity, resorting to poisson")
    ##     mod = glm(..., family = "poisson")
    ## }
    return(mod)
}


#' @name summ_glm
#'
#' Parse a tabular summary of a glm model
#'
#' @return A data.frame/data.table
#' @export
summ_glm = function(glm_mod, as.data.table = TRUE, ...) {
    this_summ = summary(glm_mod, ...)
    parse_glm_sum = function(x) {
        df = as.data.frame(x) %>%
            tibble::rownames_to_column(var = "name") %>%
            ## select(one_of(c("name", "estimate", "SE", "t.value", "p.value", "z value", "Estimate", "Std. Error", "t value", "Pr(>|t|)", "Pr(>|z|)")))
            select(matches("name"), matches("estimate"), matches("std.*error"), matches("z|t(\\.| )?value"), matches("pr\\(>\\|"))
        df = df[,c("name", intersect(colnames(x), colnames(df)))]
        if (!is.null(df[["t value"]])) {
            data.table::setnames(df, c("name", "estimate", "SE", "t.value", "p"))
        } else if (!is.null(df[["z value"]])) {
            data.table::setnames(df, c("name", "estimate", "SE", "z.value", "p"))
        }
        df = df %>% mutate_if(~inherits(., "character"), ~trimws(.))
        df
    }
    if (inherits(this_summ, c("summary.vglm", "vglm"))) {
        out = this_summ@coef3 %>% parse_glm_sum()
    } else if (inherits(this_summ$coefficients, "list")) {
        out = this_summ$coefficients %>% `[[`(1) %>% parse_glm_sum()
    } else {
        out = this_summ$coefficients %>% parse_glm_sum()
    }
    f.obj = family(glm_mod)
    fam = f.obj$family
    lin = f.obj$link
    ## if (!class(glm_mod)[1] == "lm") {
    ##     summ.ul = unlist(this_summ)
    ##     fam1 = unlist(summ.ul[names(summ.ul) == "family"])
    ##     fam2 = unlist(summ.ul[names(summ.ul) == "family.family"])
    ##     lin1 = unlist(summ.ul[names(summ.ul) == "link"])
    ##     lin2 = unlist(summ.ul[names(summ.ul) == "family.link"])
    ##     fam = trimws(paste(fam1, fam2))
    ##     lin = trimws(paste(lin1, lin2))
    ## } else {
    ##     fam = "gaussian"
    ##     lin = "identity"
    ## }
    out = mutate(out,
                 ci.lower = estimate - (1.96 * SE),
                 ci.upper = estimate + (1.96 * SE),
                 family = fam,
                 link = lin)
    if (as.data.table) {
        setDT(out)
    }
    return(out)
}


##################################################
##################################################
##################################################
##### gTrack stuff!

#' @name summ_glm
#'
#' Parse a tabular summary of a glm model
#'
#' @return A data.frame/data.table
#' @export
gt.fix = function(gt, lwd.scale = 1, lwd.border.scale = 1, ywid.scale = 1) {
    len = function(ob) if (length(ob) == 0) NULL else ob
    for(i in seq_along(gt@edges) ) {
        if (inherits(gt@edges[[i]], "data.frame") && dim(gt@edges[[i]])[1] > 0) {
            gt@edges[[i]][["lwd"]] = len(gt@edges[[i]][["lwd"]] * lwd.scale)
        }
    }
    for(i in seq_along(gt@data) ) {
        if (inherits(gt@data[[i]], "GRanges") && length(gt@data[[i]]) > 0) {
            mcols(gt@data[[i]])[["ywid"]] = len(mcols(gt@data[[i]])[["ywid"]] * ywid.scale)
            mcols(gt@data[[i]])[["lwd.border"]] = len(mcols(gt@data[[i]])[["lwd.border"]] * lwd.border.scale)
        }
    }
    gt
}



##############################
############################## ggplot2 stuff
##############################
#' @name gg_mytheme
#'
#' A custom theme for ggplots
#'
#' @return A ggplot object
#' @export
gg_mytheme = function(gg,
                      base_size = 16,
                      legend.position = "none",
                      flip_x = TRUE,
                      x_axis_cex = 1,
                      y_axis_cex = 1,
                      ylab_cex = 1,
                      xlab_cex = 1,
                      title_cex = 1,
                      x_angle = 90,
                      x_axis_hjust = 0.5,
                      x_axis_vjust = 0.5,
                      y_axis_hjust = 0.5) {
    ## gg = gg + theme_bw(base_size = base_size) + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), plot.background = element_blank(), axis.line = element_line(colour = "black"), axis.text.x  = element_text(angle = 90, vjust = .5), legend.position = legend.position)
    gg = gg +
        theme_bw(base_size = base_size) +
        theme(panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              plot.background = element_blank(),
              axis.line = element_line(colour = "black"),
              axis.text.x  = element_text(angle = x_angle, vjust = x_axis_vjust, hjust = x_axis_hjust, size = rel(x_axis_cex), colour = "black"),
              legend.position = legend.position,
              axis.text.y = element_text(size = rel(y_axis_cex), hjust = y_axis_hjust, colour = "black"),
              plot.title = element_text(size = rel(title_cex)),
              axis.title.x = element_text(size = rel(xlab_cex)),
              axis.title.y = element_text(size = rel(ylab_cex)))
    return(gg)
}


#' @name gbar.error
#'
#' A barplot of fractions with confidence intervals. To be used with
#' binom.conf.
#' 
#' @return A ggplot object
#' @export
gbar.error = function(frac, conf.low, conf.high, group, wes = "Royal1", print = TRUE, fill = NULL) {
    dat = data.table(frac = frac, conf.low = conf.low, conf.high = conf.high, group = group)
    if (is.null(fill)) fill.arg = group else fill.arg = fill
    dat[, fill.arg := fill.arg]
    gg = ggplot(dat, aes(x = group, fill = fill.arg, y = frac)) +
        geom_bar(stat = "identity", position = position_dodge(width = 0.9)) +
        geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size = 0.1, width = 0.3, position = position_dodge(width = rel(0.9)))
    if (!is.null(wes)) 
        gg = gg + scale_fill_manual(values = wesanderson::wes_palette(wes))
    if (print) print(gg) else gg
}




##############################
############################## htslib stuff
##############################

#' @name read.bam.header
#'
#' Read in a bam header into tabular format
#' 
#' @return A data.table
#' @export
read.bam.header = function(bam, trim = FALSE) {
    cmd = sprintf("samtools view -H %s", bam)
    if (!trim) {
        return(as.data.table(read.table(text = system(cmd, intern = TRUE), fill = TRUE, sep = "\t")))
    } else {
        read.table(text = system(cmd, intern = TRUE), fill = TRUE, sep = "\t") %>% filter(grepl("^SN", V2)) %>% mutate(V2 = gsub("SN:", "", V2)) %>% as.data.table
    }
}


##############################
##############################
############################## data.table and general data.frame utilities
##############################
##############################

#' @name grep_order
#'
#' order text based on the supplied order of multiple patterns
#' 
#' @return A character vector
#' @export
grep_order = function(patterns, text, return_na = FALSE, first_only = FALSE, perl = TRUE, fixed = FALSE) {
    text_ix = 1:length(text)
    match_lst = lapply(1:length(patterns), function(i) {
        these_matches = regexpr(patterns[i], text, perl = perl, fixed = fixed)
        position = which(these_matches != -1)
        if (first_only) {
            position = position[1]
        }
        if (length(position) > 0) {
            return(text_ix[position])
        } else if (return_na) {
            return(NA)
        }
    })
    return(unlist(match_lst))
}

#' @name grep_col_sort
#'
#' order columns of a data.frame/data.table based on the supplied
#' order of multiple character patterns.
#' 
#' @return A data.frame/data.table
#' @export
grep_col_sort = function(patterns, df, all_cols = TRUE, match_first = TRUE, perl = TRUE, fixed = FALSE) {
    is.data.table = FALSE
    if (inherits(df, "data.table")) {
        df = as.data.frame(df)
        is.data.table = TRUE
    }
    new_col_order = grep_order(patterns = patterns, text = colnames(df), return_na = FALSE, first_only = FALSE, perl = perl, fixed = fixed)
    if (all_cols) {
        other_cols = setdiff(1:ncol(df), new_col_order)
        if (!match_first) {
            col_ix = c(other_cols, new_col_order)
        } else {
            col_ix = c(new_col_order, other_cols)
        }
    } else {
        col_ix = new_col_order
    }
    df = df[, col_ix]
    if (is.data.table) {
        return(as.data.table(df))
    } else {
        return(df)
    }
}


#' @name merge.repl
#'
#' Merge two data tables
#' 
#' @return A data.table
#' @export merge.repl
merge.repl = function(dt.x,
                      dt.y,
                      replace_in_x = TRUE,
                      suffix = NULL,
                      sep = "_",
                      replace_NA = TRUE,
                      force_y = TRUE,
                      overwrite_x = FALSE,
                      keep_order = FALSE,
                      ...) {
    arg_lst = as.list(match.call())
    by.y = eval(arg_lst$by.y)
    by.x = eval(arg_lst$by.x)
    by = eval(arg_lst$by)
    all.x = eval(arg_lst$all.x)
    all.y = eval(arg_lst$all.y)
    all = eval(arg_lst$all)
    allow.cartesian = eval(arg_lst$allow.cartesian)
    key_x = key(dt.x)
    if (is.null(all.x)) {
        all.x = TRUE
    }
    if (is.null(all.y)) {
        all.y = FALSE
    }
    if (!is.null(all) && all) {
        all.y = TRUE
        all.x = TRUE
    }
    if (is.null(allow.cartesian)) {
        allow.cartesian = FALSE
    }
    if (!inherits(dt.x, "data.table")) {
        dt.x = as.data.table(dt.x)
    }
    if (!inherits(dt.y, "data.table")) {
        dt.y = as.data.table(dt.y)
    }
    ## data.table::set(dt.x, j = "tmp.2345098712340987", value = seq_len(nrow(dt.x)))
    ## data.table::set(dt.x, j = "in.x.2345098712340987", value = TRUE)
    ## data.table::set(dt.y, j = "in.y.2345098712340987", value = TRUE)
    if (keep_order == TRUE) {
        dt.x$tmp.2345098712340987 = seq_len(nrow(dt.x))
    }
    dt.x$in.x.2345098712340987 = TRUE
    dt.y$in.y.2345098712340987 = TRUE
    new_ddd_args = list(by = by, by.x = by.x, by.y = by.y, all.x = all.x, all.y = all.y, allow.cartesian = allow.cartesian)
    if (is.null(by.y) & is.null(by.x) & is.null(by)) {
        k.x = key(dt.x)
        k.y = key(dt.y)
        if (is.null(k.x) | is.null(k.y) || (k.x != k.y)) {
            stop("neither by.x/by.y  nor by are supplied, keys of dt.x and dt.y must be identical and non NULL")
        }
        x.cols = setdiff(names(dt.x), k.x)
        y.cols = setdiff(names(dt.y), k.y)
    } else if (!is.null(by.x) & !is.null(by.y)) {
        x.cols = setdiff(names(dt.x), by.x)
        y.cols = setdiff(names(dt.y), by.y)
        new_ddd_args = new_ddd_args[setdiff(names(new_ddd_args), c("by"))]
    } else if (!is.null(by)) {
        x.cols = setdiff(names(dt.x), by)
        y.cols = setdiff(names(dt.y), by)
        ## if (length(x.cols) == 0 | length(y.cols) == 0) {
        if (! all(by %in% colnames(dt.x)) | ! all(by %in% colnames(dt.y))) {
            stop("column ", by, " does not exist in one of the tables supplied \nCheck the column names")
        }
        new_ddd_args = new_ddd_args[setdiff(names(new_ddd_args), c("by.y", "by.x"))]

    }
    these_cols = intersect(x.cols, y.cols)
    if (replace_in_x) {
        if (!replace_NA) {
            ## dt.x.tmp = copy(dt.x)[, eval(dc(these_cols)) := NULL]
            dt.x.tmp = copy(dt.x)
            for (this_col in these_cols) {
                data.table::set(dt.x.tmp, i = NULL, j = this_col, value = NULL)
            }
            ## dt.repl = merge(dt.x.tmp, dt.y, all.x = all.x, ...)
            dt.repl = do.call("merge", args = c(list(x = dt.x.tmp, y = dt.y), new_ddd_args))
            dt_na2false(dt.repl, c("in.x.2345098712340987", "in.y.2345098712340987"))
        } else {
            ## dt.repl = merge(dt.x, dt.y, all.x = all.x, ...)
            dt.repl = do.call("merge", args = c(list(x = dt.x, y = dt.y), new_ddd_args))
            dt_na2false(dt.repl, c("in.x.2345098712340987", "in.y.2345098712340987"))
            this_env = environment()
            for (this_col in these_cols) {
                x_cname = paste0(this_col, ".x")
                y_cname = paste0(this_col, ".y")
                ## x_col = as.data.frame(dt.repl)[, x_cname]
                x_col = dt.repl[[x_cname]]
                ## y_col = as.data.frame(dt.repl)[, y_cname]
                y_col = dt.repl[[y_cname]]
                if (force_y) {
                    if (!overwrite_x) {
                        if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                            new_col = factor(y_col, forcats::lvls_union(list(y_col, x_col)))
                            new_col[is.na(new_col)] = x_col[is.na(new_col)]
                        } else {
                            new_col = ifelse(!is.na(y_col), y_col, x_col)
                        }
                    } else {
                        if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                            new_col = factor(x_col, forcats::lvls_union(list(y_col, x_col)))
                        } else {
                            new_col = x_col
                        }
                        new_col[dt.repl$in.y.2345098712340987] = y_col[dt.repl$in.y.2345098712340987]
                        ## new_col = y_col
                    }
                } else {
                    if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                        new_col = factor(x_col, forcats::lvls_union(list(x_col, y_col)))
                        new_col[is.na(new_col) & !is.na(y_col)] = y_col[is.na(new_col) & !is.na(y_col)]
                    } else {
                        new_col = ifelse(is.na(x_col) & !is.na(y_col), y_col, x_col)
                    }
                }
                ## dt.repl[, eval(dc(c(x_cname, y_cname))) := NULL]
                data.table::set(dt.repl, j = c(x_cname, y_cname, this_col), value = list(NULL, NULL, this_env[["new_col"]]))
            }
        }
    } else if (!replace_in_x & !is.null(suffix)) {
        y.suff.cols = paste0(y.cols, sep, suffix)
        ## dt.y.tmp = copy(dt.y)[, eval(dc(y.suff.cols)) := eval(dl(y.cols))][, eval(dc(y.cols)) := NULL]
        dt.y.tmp = copy(dt.y)
        data.table::set(dt.y, j = y.suff.cols, value = dt.y[, y.cols, with = FALSE])
        data.table::set(dt.y, j = y.cols, value = NULL)
        ## dt.repl = merge(dt.x, dt.y.tmp, all.x = TRUE, ...)
        dt.repl = do.call("merge", args = c(list(x = dt.x, y = dt.y.tmp), new_ddd_args))
    }
    if (keep_order == TRUE) {
        data.table::setorderv(dt.repl, "tmp.2345098712340987")
        dt.repl$tmp.2345098712340987 = NULL
    }
    data.table::set(dt.repl, j = c("in.y.2345098712340987", "in.x.2345098712340987"),
                    value = list(NULL, NULL))
    invisible(dt.repl)
}


#' @export
dt_lg2int = function(dt) {
    these_cols = which(sapply(dt, class) == "logical")
    for (this_col in these_cols) {
        this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        data.table::set(dt, j = this_col, value = as.integer(this_val))
    }
    return(dt)
}

#' @export
dt_na2false = function(dt, these_cols = NULL) {
    na2false = function(v)
    {
        ## v = ifelse(is.na(v), v, FALSE)
        v[is.na(v)] = FALSE
        as.logical(v)
    }
    if (is.null(these_cols)) {
        these_cols = which(sapply(dt, class) == "logical")
    }
    for (this_col in these_cols) {
        ## this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        this_val = dt[[this_col]]
        data.table::set(dt, j = this_col, value = na2false(this_val))
    }
    return(dt)
}

#' @export
dt_na2true = function(dt, these_cols = NULL) {
    if (is.null(these_cols)) {
        these_cols = which(sapply(dt, class) == "logical")
    }
    for (this_col in these_cols) {
        ## this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        this_val = dt[[this_col]]
        data.table::set(dt, j = this_col, value = na2true(this_val))
    }
    return(dt)
}

#' @export
dt_na2zero = function(dt, these_cols = NULL) {
    if (is.null(these_cols)) {
        these_cols = which(sapply(dt, class) %in% c("numeric", "integer"))
    }
    if (!inherits(dt, "data.table")) {
        setDT(dt)
    }
    for (this_col in these_cols) {
        this_val = dt[[this_col]]
        ## this_val = as.data.frame(dt)[, this_col]
        this_val[is.na(this_val)] = 0
        data.table::set(dt, j = this_col, value = this_val)
        ## dt[, this_col] = this_val
    }
    return(dt)
}

#' @export
dt_na2empty = function(dt) {
    these_cols = which(sapply(dt, class) == "character")
    for (this_col in these_cols) {
        ## this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        this_val = dt[, this_col, with = FALSE][[1]]
        data.table::set(dt, j = this_col, value = na2empty(this_val))
    }
    return(dt)
}

#' @export
dt_empty2na = function(dt) {
    these_cols = which(sapply(dt, class) == "character")
    for (this_col in these_cols) {
        ## this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        this_val = dt[, this_col, with = FALSE][[1]]
        ## browser()
        data.table::set(dt, j = this_col, value = empty2na(this_val))
    }
    return(dt)
}

#' @export
dt_setnull = function(dt, cols) {
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = NULL)
    }
    return(dt)
}

#' @export
dt_setint = function(dt, cols = NULL) {
    if (is.null(cols)) {
        cols = names(dt)[which(sapply(dt, class) %in% c("numeric"))]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.integer(dt[[this_col]]))
    }
    return(dt)
}


#' @export
dt_setallna = function(dt, cols = NULL, na_type = NA_integer_) {
    if (is.null(cols)) {
        cols = colnames(dt)
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = NULL)
        data.table::set(dt, j = this_col, value = na_type)
    }
    return(dt)
}

#' @export
dt_setchar = function(dt, cols = NULL) {
    if (is.null(cols)) {
        cols = names(dt)[which(!sapply(dt, class) == "character")]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.character(dt[[this_col]]))
    }
    return(dt)
}

#' @export
dt_any2lg = function(dt, cols = NULL) {
    if (is.null(cols)) {
        ## cols = names(dt)[which(!unlist(lapply(dt, class)) == "character")]
        cols = colnames(dt)
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.logical(dt[[this_col]]))
    }
    return(dt)
}

#' @export
dt_f2char = function(dt, cols = NULL) {
    if (is.null(cols)) {
        ## cols = names(dt)[which(!unlist(lapply(dt, class)) == "character")]
        cols = colnames(dt)
    } else {
        cols = names(dt)[which(!unlist(lapply(dt, class)) == "factor")]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.character(dt[[this_col]]))
    }
    return(dt)
}


##############################
##############################
############################## Genomics / mskilab stuff
##############################
##############################

#' @name within
#' @title within on GRanges
#' @description
#'
#'
#' @return GRanges
#' @rdname gr.within
#' @exportMethod within
#' @aliases within,GRanges-method
#' @author Kevin Hadi
setMethod("within", signature(data = "GRanges"), function(data, expr) {
    top_prenv1 = function (x, where = parent.frame()) 
    {
        sym <- substitute(x, where)
        if (!is.name(sym)) {
            stop("'x' did not substitute to a symbol")
        }
        if (!is.environment(where)) {
            stop("'where' must be an environment")
        }
        .Call2("top_prenv", sym, where, PACKAGE = "S4Vectors")
    }
    e <- list2env(as.list(as(data, "DataFrame")))
    e$X = NULL
    e$data <- granges(data)
    S4Vectors:::safeEval(substitute(expr, parent.frame()), e, top_prenv1(expr))
    reserved <- c("seqnames", "start", "end", "width", "strand", "data")
    l <- mget(setdiff(ls(e), reserved), e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(colnames(mcols(data)), (nl <- names(l))))
    mcols(data) = l
    if (nD) {
        for (nm in del)
            mcols(data)[[nm]] = NULL
    }
    if (!identical(granges(data), e$data)) {
        granges(data) <- e$data
    }
    data
})


setMethod("within", signature(data = "GRangesList"), function(data, expr) {
    top_prenv1 = function (x, where = parent.frame()) 
    {
        sym <- substitute(x, where)
        if (!is.name(sym)) {
            stop("'x' did not substitute to a symbol")
        }
        if (!is.environment(where)) {
            stop("'where' must be an environment")
        }
        .Call2("top_prenv", sym, where, PACKAGE = "S4Vectors")
    }
    e <- list2env(as.list(as(data, "DataFrame")))
    e$X = NULL
    e$data <- gr.noval(data)
    S4Vectors:::safeEval(substitute(expr, parent.frame()), e, top_prenv1(expr))
    ## reserved <- c("ranges", "start", "end", "width", "space")
    reserved <- c("seqnames", "start", "end", "width", "strand", "data")
    l <- mget(setdiff(ls(e), reserved), e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(colnames(mcols(data)), (nl <- names(l))))
    mcols(data) = l
    if (nD) {
        for (nm in del)
            mcols(data)[[nm]] = NULL
    }
    if (!identical(gr.noval(data), e$data)) {
        stop("change in the grangeslist detected")
        ## granges(data) <- e$granges
    } ## else {
    ##     if (!identical(start(data), start(e$grangeslist)))
    ##         start(data) <- start(e$grangeslist)
    ##     if (!identical(end(data), end(e$grangeslist)))
    ##         end(data) <- end(e$grangeslist)
    ##     if (!identical(width(data), width(e$grangeslist)))
    ##         width(data) <- width(e$grangeslist)
    ## }
    data
})


#' @export
en2DF = function(ed, nodes, from_field = "from", to_field = "to", strand_sign = NULL, from_strand_sign = NULL, to_strand_sign = NULL, from_strand = NULL, to_strand = NULL) {
    edf = S4Vectors::DataFrame(from = ed[[from_field]], to = ed[[to_field]], from.gr = unname(nodes[ed[[from_field]]][,c()]), to.gr = unname(nodes[ed[[to_field]]][,c()]))
    if (!is.null(strand_sign)) {
        from_new_strand = ifelse(strand_sign > 0, c("+" = "+", "-" = "-")[as.character(strand(edf$from.gr))], ifelse(strand_sign < 0, c("-" = "+", "+" = "-")[as.character(strand(edf$from.gr))], "*"))
        to_new_strand = ifelse(strand_sign > 0, c("+" = "+", "-" = "-")[as.character(strand(edf$to.gr))], ifelse(strand_sign < 0, c("-" = "+", "+" = "-")[as.character(strand(edf$to.gr))], "*"))
        edf$from.gr = gr.strand(edf$from.gr, from_new_strand)
        edf$to.gr = gr.strand(edf$to.gr, to_new_strand)
    }
    if (!is.null(from_strand_sign)) {
        from_new_strand = ifelse(from_strand_sign > 0, c("+" = "+", "-" = "-")[as.character(strand(edf$from.gr))], ifelse(from_strand_sign < 0, c("-" = "+", "+" = "-")[as.character(strand(edf$from.gr))], "*"))
        edf$from.gr = gr.strand(edf$from.gr, from_new_strand)
    }
    if (!is.null(to_strand_sign)) {
        to_new_strand = ifelse(to_strand_sign > 0, c("+" = "+", "-" = "-")[as.character(strand(edf$to.gr))], ifelse(to_strand_sign < 0, c("-" = "+", "+" = "-")[as.character(strand(edf$to.gr))], "*"))
        edf$to.gr = gr.strand(edf$to.gr, to_new_strand)
    }
    if (!is.null(from_strand)) {
        edf$from.gr = gr.strand(edf$from.gr, from_strand)
    }
    if (!is.null(to_strand)) {
        edf$to.gr = gr.strand(edf$to.gr, to_strand)
    }
    rownames(edf) = as.character(seq_len(nrow(ed)))
    edf$bp.left = gr.end(edf$from.gr, width = 1, ignore.strand = FALSE)[,c()]
    edf$bp.right = gr.start(edf$to.gr, width = 1, ignore.strand = FALSE)[,c()]
    strand(edf$bp.left) = ifelse(as.character(strand(edf$from.gr)) == "-", "+", "-")
    strand(edf$bp.right) = ifelse(as.character(strand(edf$to.gr)) == "-", "-", "+")
    edf$junction = grl.pivot(GRangesList(edf$bp.left, edf$bp.right))
    edf$ref = as.logical(ifelse(seqnames(edf$bp.left) == seqnames(edf$bp.right), abs(start(edf$bp.left) - end(edf$bp.right)), Inf) == 1 &
        strand(edf$bp.left) != strand(edf$bp.right))
    return(edf)
}

#' @export
map_fus2unfus = function(ed, nodes, exact = TRUE) {
    if (!inherits.edf(ed)) {
        if (is.null(nodes)) {
            stop("Nodes must be supplied with ed to ascertain coordinates and junctions")
        }
        ed = en2DF(ed = ed, nodes = nodes)
    }
    if (is.null(ed$from.unfus.junc) | is.null(ed$to.unfus.junc)) {
        ed$bp.left.unfus.gr = gr.shift(gr.end(ed$from.gr, ignore.strand = FALSE), 1, ignore.strand = FALSE)
        ed$bp.right.unfus.gr = gr.flipstrand(gr.shift(gr.start(ed$to.gr, ignore.strand = FALSE), -1, ignore.strand = FALSE))
        ed$from.unfus.junc = grl.pivot(GRangesList(ed$bp.left, ed$bp.left.unfus.gr))
        ed$to.unfus.junc = grl.pivot(GRangesList(ed$bp.right, ed$bp.right.unfus.gr))
    }

    match_mat_ufrom = ra.overlaps2(ed$junction, ed$from.unfus.junc);
    setnames(match_mat_ufrom, c("ra1.ix", "ra2.ix")); match_mat_ufrom = as.matrix(match_mat_ufrom)
    match_mat_ufrom = match_mat_ufrom[gr.poverlaps(ed[match_mat_ufrom[,1],]$from.gr, ed[match_mat_ufrom[,2],]$from.gr),,drop = FALSE]
    match_mat_uto = ra.overlaps2(ed$junction, ed$to.unfus.junc, pad = 2); setnames(match_mat_uto, c("ra1.ix", "ra2.ix")); match_mat_uto = as.matrix(match_mat_uto)
    match_mat_uto = match_mat_uto[gr.poverlaps(ed[match_mat_uto[,1],]$to.gr, ed[match_mat_uto[,2],]$to.gr),,drop = FALSE]

    ed$from.unfus = as.integer(NA)
    ed$to.unfus = as.integer(NA)
    ed[match_mat_uto[,2],]$from.unfus = ed[match_mat_uto[,1],]$from
    ed[is.na(ed$from.unfus),][["from.unfus"]] = ed[is.na(ed$from.unfus),]$from ## loose ends -- just make unfus side the same

    ed[match_mat_ufrom[,2],]$to.unfus = ed[match_mat_ufrom[,1],]$to
    ed[is.na(ed$to.unfus),][["to.unfus"]] = ed[is.na(ed$to.unfus),]$to ## loose ends -- just make the unfus the same

    ed$from.unfus.gr = nodes[ed[["from.unfus"]]]
    ed$to.unfus.gr = nodes[ed[["to.unfus"]]]
    ## if (all(dim(match_mat) == c(1,2)) & all(is.na(match_mat)))  {
    ##     return(match_mat_)
    ## }
    return(ed)
}

#' @name ra.overlaps6
#'
#' One of the many rewrites of ra.overlaps
#' 
#' @export
ra.overlaps6 = function(ra1, ra2, pad = 0) {
    ra1 = gr.noval(ra1)
    ra2 = gr.noval(ra2)
    bp1 = grl.unlist(ra1) + pad
    bp2 = grl.unlist(ra2) + pad
    ix2 = unname(plyranges::find_overlaps_directed(bp1, bp2))
    ix2 = gr2dt(ix2)
    ix2[, ra.match := all(c(1,2) %in% grl.iix.x & all(c(1,2) %in% grl.iix.y)), by = .(grl.ix.x, grl.ix.y)]
    ix2 = ix2[ra.match == TRUE][!duplicated(data.table(grl.ix.x, grl.ix.y))]
    ix2[, cbind(grl.ix.x, grl.ix.y)]
}



#' filter sv by overlaps with another
#'
#' To filter out a grangeslist of sv by another grangeslist of SV
#'
#' @param sv GRangesList with all elements length 2 (specifying breakpoint pairs of a junction)
#' @param filt_sv GRangesList with all elements length 2 (usually a pon)
#' @param pad Exposed argument to skitools::ra.overlaps()
#' @return GRangesList of breakpoint pairs with junctions that overlap removed
#' @export
sv_filter = function(sv, filt_sv, pad = 500)
{
    ## within_filt = ra.overlaps2(sv, filt_sv, pad = pad)
    if (length(sv) == 0) {
        return(sv)
    }
    within_filt = suppressWarnings(ra.overlaps6(sv, filt_sv, pad = pad))
    ## filter_these = unique(within_tcga_germ[,"ra1.ix"])
    ## filter_these = unique(within_filt[,1, with = FALSE][[1]])
    filter_these = unique(within_filt[,1])
    sv = ix_sdiff(sv, filter_these)
    return(sv)
}



#' @name .filter_sv
#'
#' perform sv filtering on a pairs entry
#'
#' @export
.filter_sv = function(ent, overwrite = FALSE) {
    if (file.exists(ent$svaba_unfiltered_somatic_vcf)) {
        outpath = paste0(file_path_sans_ext(ent$svaba_unfiltered_somatic_vcf), ".pon.filtered.rds")
        if (isTRUE(overwrite) || isFALSE(file.exists(outpath))) {
            sv = JaBbA::read.junctions(ent$svaba_unfiltered_somatic_vcf)
            if (!exists("sv_pon")) {
                sv_pon = gr.noval(readRDS('~/lab/projects/CCLE/db/tcga_and_1kg_sv_pon.rds'))
            }
            sv = sv_filter(sv, sv_pon, pad = 1000)
            outpath = paste0(file_path_sans_ext(ent$svaba_unfiltered_somatic_vcf), ".pon.filtered.rds")
            saveRDS(sv, outpath, compress = FALSE)
            message(ent$pair, " finished")
            message("\n")
            data.table(pair = ent$pair, svaba_unfiltered_somatic_vcf_sv_pon_filtered = outpath)
        } else if (isTRUE(file.exists(outpath))) {
            data.table(pair = ent$pair, svaba_unfiltered_somatic_vcf_sv_pon_filtered = outpath)
        }
    } else {
        data.table(pair = ent$pair, svaba_unfiltered_somatic_vcf_sv_pon_filtered = NA_character_)
    }
}

#' @export pairs.filter.sv
pairs.filter.sv = function(tbl, id.field, sv.field = "svaba_unfiltered_somatic_vcf", mc.cores = 1, pon.path = '~/lab/projects/CCLE/db/tcga_and_1kg_sv_pon.rds') {
    if (missing(id.field))
        id.field = key(tbl)
    if (is.null(id.field))
        stop("please specify an id field")
    if (!exists("sv_pon")) {
        message("no sv_pon variable found...", "\n",
                "loading ", pon.path)
        sv_pon = gr.noval(readRDS(pon.path))
    }
    iter.fun = function(pr, tbl) {
        ent = tbl[get(id.field) == pr]
        .filter_sv(ent)
    }
    out = rbindlist(mclapply(mc.cores = mc.cores,
                   tbl[[id.field]], iter.fun, tbl = tbl))
}


#' @export plot.jabba
plot.jabba = function(pairs, win, filename, use.jab.cov = TRUE, field.name = "jabba_rds", cov.field.name = "cbs_cov_rds", cov.y.field = "ratio", title = "", ...) {
    gg = gG(jabba = pairs[[field.name]])
    if (isTRUE(use.jab.cov))
        cov = readRDS(inputs(readRDS(pairs[[field.name]] %>% dig_dir("Job.rds$")))$CovFile)
    else
        cov = readRDS(pairs[[cov.field.name]])
    gcov = gTrack(cov, cov.y.field, circles = TRUE, lwd.border = 0.0001)
    if (missing(win))
        win = si2gr(gg) %>% keepStandardChromosomes(pruning.mode = "coarse") %>% gr.sort
    if (missing(filename))
        ppng(plot(c(gcov, gg$gtrack()), win = win, ...), res = 200, title = title)
    else
        ppng(plot(c(gcov, gg$gtrack()), win = win, ...), filename = filename, res = 200, title = title)
}

#' @export pairs.plot.jabba
pairs.plot.jabba = function(pairs, dirpath = "~/public_html/jabba_output", jabba.field = "jabba_rds", cov.y.field = "foreground", id.field = "pair", mc.cores = 1) {
    paths = subset2(pairs[[jabba.field]], file.exists(x))
    iter.fun = function(x, tbl) {
        ent = tbl[get(jabba.field) == x]
        ttl = ent[[id.field]]
        plot.jabba(ent, use.jab.cov = TRUE, filename = paste0(dirpath, "/", ent[[id.field]], ".png"), cov.y.field = cov.y.field, y.quantile = 0.01, title = ttl)
    }
    mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores)
    NULL
}

#' @export pairs.process.events
pairs.process.events = function(pairs, events.field = "complex", id.field = "pair", mc.cores = 1) {
    paths = subset2(pairs[[events.field]], file.exists(x))
    iter.fun = function(x, tbl) {
        ent = pairs[get(events.field) == x]
        gg = readRDS(ent[[events.field]])
        out = copy(gg$meta$events)
        set(out, j = id.field, value = ent[[id.field]])
        out
    }
    evs = rbindlist(mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores), fill = TRUE)
    fid = evs[, factor(get(id.field), levels = pairs[get(events.field) %in% paths][[id.field]])]
    set(evs, j = paste0("f", id.field), value = fid)
}

#' @export pairs.collect.junctions
pairs.collect.junctions = function(pairs, jn.field = "complex", id.field = "pair", mc.cores = 1, mask = '/gpfs/commons/groups/imielinski_lab/DB/Broad/um75-hs37d5.bed.gz') {
    paths = subset2(pairs[[jn.field]], file.exists(x))
    mask = rtracklayer::import(mask)
    iter.fun = function(x, tbl) {
        ent = tbl[get(jn.field) == x]   
        .fun = function(gg) {
            ## dd.ov = gr.sum(gg$edges[type == "ALT"][class %in% c("DEL-like", "DUP-like")]$shadow) %Q% (score > 1)
            gg.alt.edge = gg$edges[type == "ALT"]
            if (length(gg.alt.edge) > 0) {
                tra_like = gg.alt.edge$dt[,class == "TRA-like"]
                gg.shad = gg.alt.edge$shadow
                gg.shad[tra_like] = gg.shad[tra_like] + 1e6
                dd.ov = gr.sum(gg.shad) %Q% (score > 1)
                gg.shad = gg.shad %>% split(.$id)
                gg$edges[type == "ALT"]$mark(overlapped = gg.shad %^% dd.ov)
            }
            gg
        }
        message("processing ", ent[[id.field]])
        cx = readRDS(ent[[jn.field]])
        if (!length(cx)) return(NULL)
        cx = .fun(cx)
        cx$edges$mark(jspan = cx$edges$span)
        cx$edges$mark(shadow = grl.string(cx$edges$shadow %>% split(.$id)))
        cx$edges$mark(sv.in.mask = grl.in(cx$edges$grl, mask, logical = FALSE) > 0)
        ## cx$edges[edge.id %in% these_id]$mark(within_node_cluster = TRUE)
        ## these_id = cx$nodes[!is.na(cluster)]$edges$dt$edge.id
        out = copy(gr2dt(grl.unlist(cx$edges[type == "ALT"]$grl)))
        if (!is.null(dim(out)) && !dim(out)[1] == 0) {
            set(out, j = "pair", value = ent[[id.field]])
            tmp = cx$.__enclos_env__$private$pedges
            ## tmp = tmp[order(edge.id)][order(abs(sedge.id))]
            ## snode = copy(cx$.__enclos_env__$private$pnodes)
            snode = cx$.__enclos_env__$private$pnodes
            ## this = as.data.frame(tmp)
            DF = map_fus2unfus(ed = tmp, nodes = snode)
            DF$edge.id = tmp$edge.id
            DF$sedge.id = tmp$sedge.id
            df = as.data.frame(lapply(DF, function(x) {
                if (inherits(x, "GRanges")) {
                    gr.string(x)
                }  else if (inherits(x, "GRangesList")) {
                    grl.string(x)
                } else {
                    x
                }
            }))
            seg_cn = df %>% filter(ref == FALSE) %>% {
                data.table(edge.id = .$edge.id,
                           max_scn = pmax(snode[.$from]$cn, snode[.$to]$cn),
                           min_scn = pmin(snode[.$from]$cn, snode[.$to]$cn))
            } %>% distinct(edge.id, .keep_all = TRUE)
            out = dplyr::left_join(out, seg_cn, by = "edge.id") %>% setDT(key = "sedge.id")
            out = gr2dt(gr.val(df2gr(out),
                               select(cx$nodes$gr, bp_scn = cn), "bp_scn"))
            out = df2gr(out) %>% mutate(bp.in.mask = (.) %^% mask) %>% gr2dt
        }
        return(out)
    }
    cx.edt = rbindlist(mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores), fill = TRUE)
    set(cx.edt, j = "fpair", value = cx.edt[, factor(get(id.field), levels = pairs[get(jn.field) %in% paths][[id.field]])])
    cx.edt = merge.repl(cx.edt, unique(cx.edt[, .(pair, edge.id, simple_type = gsub("([A-Z]+)([0-9]+)", "\\1", simple), simple_num = gsub("([A-Z]+)([0-9]+)", "\\2", simple))]), by = c("pair", "edge.id"))
    cx.edt[, simple_type := fct_explicit_na(simple_type, "NA")]
    mod.dt = mltools::one_hot(cx.edt[, .(simple_type)])
    cx.edt = cbind(select(cx.edt, -matches("^simple_.*$")),
                   rename_all(mod.dt, ~paste0("simple", gsub("simple_type", "", tolower(.)))))
    ev.types = c("bfb", "chromoplexy", "chromothripsis", "del", "dm", "dup", "fbi", "pyrgo", "qrp", "rigma", "simple_inv", "simple_invdup", "simple_tra", "tic", "tyfonas")
    cx.mat = as.matrix(mutate_all(replace_na(cx.edt[, ev.types,with = FALSE], 0), as.numeric))
    cx.mat = cx.mat > 0
    mode(cx.mat) = "integer"
    cx.edt[, unclassified := rowSums(cx.mat) == 0]
    return(cx.edt)
}


#' @export pairs.jabba.opt.report
pairs.jabba.opt.report = function(pairs, jabba.field = "jabba_rds", id.field = "pair", mc.cores = 1) {
    pairs= copy(pairs)
    pairs$opt.report = dig_dir(pairs[[jabba.field]], "opt.report.rds")
    paths = subset2(pairs$opt.report, file.exists(x))
    iter.fun = function(x, tbl) {
        ent = tbl[opt.report == x]
        id = ent[[id.field]]
        jab.path = ent[[jabba.field]]
        out = readRDS(x)
        out[[id.field]] = id
        out = copy(out)
        gg = gG(jabba = jab.path)
        converged = gg$nodes$dt[, sum(width[epgap < 0.1], na.rm = T) / sum(width, na.rm = T)]
        out$converged = converged
        out
    }
    out = rbindlist(mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores), fill = TRUE)
    setkeyv(out, id.field)
}

#' @export pairs.diagnose.jabba
pairs.diagnose.jabba = function(pairs, jabba.field = "jabba_rds", id.field = "pair", mc.cores = 1) {
    pairs= copy(pairs)
    pairs$opt.report = dig_dir(pairs[[jabba.field]], "opt.report.rds")
    paths = subset2(pairs$opt.report, file.exists(x))
    iter.fun = function(x, tbl) {
        ent = tbl[opt.report == x]
        id = ent[[id.field]]
        jab.path = ent[[jabba.field]]
        out = readRDS(x)
        out[[id.field]] = id
        out = copy(out)
        gg = gG(jabba = jab.path)
        kag = gG(jabba = readRDS(dig_dir(jab.path, "karyograph.rds$")))
        kag.rds = dig_dir(jab.path, "karyograph.rds$")
        ppfit.png = normalizePath(dig_dir(jab.path, "karyograph.rds.ppfit.png"))
        gg.raw = gG(jabba = readRDS(dig_dir(jab.path, "jabba.raw.rds")))
        converged = gg$nodes$dt[, sum(width[epgap < 0.1], na.rm = T) / sum(width, na.rm = T)]
        mat1 = as.matrix(factor(kag$nodes$gr$var < 0, levels = c(FALSE, TRUE)) %>% table3) %>% t
        colnames(mat1) = c("posvar", "negvar", "navar")
        top3conv = head(out, 3)[, all(epgap < 0.1)]
        mat2 = t(as.matrix(gg.raw$nodes$dt$cn.fix %>% is.na %>% table))
        colnames(mat2) = c("fixed", "unfixed")
        summary.stat = data.table(conv = converged) %>% cbind(mat1, top3conv = top3conv, mat2)
        summary.stat[[id.field]] = id
        summary.stat = copy(summary.stat)[, ppfit.png := ppfit.png][, kag.rds := kag.rds]
        summary.stat
    }
    out = rbindlist(mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores), fill = TRUE)
    setkeyv(out, id.field)
}

#' @export pairs.get.jabba.pp
pairs.get.jabba.pp = function(pairs, jabba.field = "jabba_rds", id.field = "pair", mc.cores = 1) {
    paths = subset2(pairs[[jabba.field]], file.exists(x))
    iter.fun = function(x, tbl) {
        ent = tbl[get(jabba.field) == x]
        purity = readRDS(ent[[jabba.field]])$purity
        ploidy = gG(jabba = ent[[jabba.field]])$nodes$dt[, sum(width * cn, na.rm = T) / sum(width, na.rm = T)]
        out = setnames(data.table(id.field = ent[[id.field]], purity, ploidy), "id.field", id.field)
        out
    }
    setkeyv(rbindlist(mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores), fill = TRUE), id.field)
}


