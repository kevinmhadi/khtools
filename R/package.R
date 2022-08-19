#' @importMethodsFrom S4Vectors with
#' @importMethodsFrom gUtils %&%
#' @importMethodsFrom S4Vectors as.data.frame
#' @importMethodsFrom S4Vectors split
#' @import tools
#' @exportMethod with

#' @name match_s
#' @title match x indices in terms of y
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

#' @name match2
#' @title matches x in terms of y
#'
#' returns vector of indices of matches in x with length of vector = length(y)
#' non matches are NA
#'
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

#' @name find_dups
#' @title find all duplicates in a vector
#'
#' @param vec A vector
#' @return a logical vector with all positions marked TRUE being duplicates
#' @examples
#' find_dups(c(1,1,1,3,5))
#' find_dups(c(1,3,1,3,1))
#' find_dups(c(3,1,5,4,4))
#'
#' @export
find_dups = function(..., re_sort = FALSE, sep = " ", as.logical = FALSE) {
  lst = as.list(match.call())[-1]
  ix = setdiff(seq_along(lst), which(names(lst) %in% c("re_sort", "sep", "as.logical")))
  ## cl = sapply(lst[ix], class)
  if (length(ix) > 1)
    vec = do.call(function(...) paste(..., sep = sep), list(...))
  else
      vec = unlist(list(...))
  duplg = duplicated(vec)
  if (as.logical) return(duplg)
  dupix = which(duplg); rm(duplg)
  if (!re_sort) {
    return(which(vec %in% vec[dupix]))
  } else {
    matching_idx = match2(sort(vec[dupix]), vec)
    return(which(!is.na(matching_idx))[order(na.omit(matching_idx))])
  }
}

#' @name undup
#' @title an alternative to base::unique() that preserves names
#'
#' @param obj an R vector
#' @return unique values of obj with names preserved
#' @export
undup = function(obj, fromLast = FALSE, nmax = NA, na.rm = FALSE) {
  dupid = which(duplicated(obj, fromLast = fromLast, nmax = NA))
  if (isTRUE(na.rm)) {
    naid = which(is.na(obj))
    dupid = union(naid, dupid)
  }
  if (NROW(dupid))
    return(obj[-dupid])
  else
    return(obj)
}



#' @name selfname
#'
#' @title name a character vector to itself
#' @param char A character vector
#' @return A named character vector
#' @export
selfname = function(char) {setNames2(char, char)}

#' @name check_lst
#' @title checking list for elements that are errors
#'
#' checking a list for any elements that are try-errors
#' usually from an lapply(..., function(x) try({})) call
#'
#' @param lst A list
#' @return a logical vector marking which elements are try-errors"
#' @export
check_lst = function(lst, class_condition = c("simpleError", "try-error", "error", "errored", "err"))
{
    ## unlist(lapply(lst, function(x) class(x)[1])) %in% class_condition
    return(vapply(lst, function(x) class(x)[1], "") %in% class_condition)
}

#' @name iderr
#' @title returns ids of list elements that are errors
#'
#' checking a list for any elements that are try-errors
#' usually from an lapply(..., function(x) try({})) call
#'
#' @param lst A list
#' @return a logical vector marking which elements are try-errors"
#' @export
iderr = function(lst, class_condition = c("simpleError", "try-error", "error", "errored", "err")) {
  which(check_lst(lst))
}

#' @name whicherr
#' @title iderr
#'
#' checking a list for any elements that are try-errors
#' usually from an lapply(..., function(x) try({})) call
#'
#' @param lst A list
#' @return a logical vector marking which elements are try-errors"
#' @export
whicherr = iderr


#' @name ret_no_err
#' @title a wrapper around check_lst
#'
#' 
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return only returns the non-errors in the list
#' @export
ret_no_err = function(lst, class_condition = c("simpleError", "try-error", "error", "errored", "err"))
{
    return(lst[!check_lst(lst, class_condition = class_condition)])
}

#' @name ret_err
#' @title a wrapper around check_lst
#'
#'
#' @param lst A list (usually the output of lapply(... , function(x) try({}))
#' @return only returns the errors in the list
#' @export
ret_err = function(lst, class_condition = c("simpleError", "try-error", "error", "errored", "err"))
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


#' @name setColnames
#' @title convenience function to set column names
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return colnamed object
#' @export
setColnames = function(object = nm, nm = NULL, pattern = NULL, replacement = "") {
    if (!is.null(nm)) {
        if (is.null(names(nm)))
            colnames2(object)  = nm
        else {
            ix = match3(names(nm), colnames(object))
            colnames2(object)[ix] = nm
        }
    } else if (!is.null(pattern)) {
        colnames2(object) = gsub(pattern, replacement, colnames2(object))
    }
    return(object)
}

#' @name setcolnames
#' @title convenience function to set column names
#'
#' alias of setColnames
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return colnamed object
#' @export
setcolnames = setColnames


#' @name setRownames
#' @title convenience function to set row names
#'
#' sets rownames of an object
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return rownamed object
#' @export
setRownames = function(object = nm, nm) {
    rownames2(object) = nm
    object
}


#' @name setrownaes
#' @title convenience function to set row names
#'
#' sets rownames of an object
#'
#' @param object tabled object
#' @param nm names of the new columns
#' @return rownamed object
#' @export
setrownames = setRownames


#' @name setcols
#' @title convenience function to set columns
#'
#' sets columns of an object
#'
#' @param dt data frame/table or matrix
#' @param old integer or character or logical vector corresponding to current colnames in dt
#' @param new character vector for new column names
#' @return colnamed object
#' @export
setcols = function(dt, old, new) {
  if (inherits(dt, c("GRanges", "GRangesList"))) {
    mcols(dt) = setcols(mcols(dt), old, new)
    return(dt)
  }
  cnames = colnames2(dt)
  if (missing(new) || missing(old)) {
    if (missing(old)) {
      old = new
    }
    if (is.character(old) && length(old) == length(cnames)) {
      colnames(dt) = old
      return(dt)
    } else {
      stop("names provided must be same length as ncol(dt)")
    }
  }
  if (is.character(old)) {
    out = merge(data.frame(cnames, seq_along(cnames)), data.frame(cnames = old, new = new),
      allow.cartesian = T)
    cnames[out[[2]]] = out[[3]]
    colnames(dt) = cnames
    return(dt)
  }
  if (is.logical(old)) {
    if (! length(old) == length(cnames)) stop("logical vector must be same length as ncol(dt)")
    old = which(old)
  }
  cnames[old] = new
  colnames(dt) = cnames
  return(dt)
}


#' @name setAllNames
#' @title setAllNames
#'
#' convenience function to set all names of a vector
#'
#' @param vec vector
#' @param nm character
#' @return named vector
#' @export
setAllNames = function(vec, nm) {
    if (is.null(nm)) {
        return(setNames(vec, NULL))
    } else {
        if (length(nm) < length(vec)) {
            nm = rep_len2(nm, vec)
        }
    }
    return(setNames(vec, nm))
}

#' @name setNames2
#' @title setNames2
#'
#' convenience function to set all names of a vector
#'
#' @param vec vector
#' @param nm character
#' @return named vector
#' @export
setNames2 = function(vec, nm, useempty = FALSE) {
    names2(vec, useempty = useempty) = nm
    return(vec)
}


#' @name intercalate
#' @title collate two vectors together
#'
#' interleave vectors together
#'
#' @param ... A set of vectors to collate
#' @return a vector with values of inputs collated together
#' @examples
#' intercalate(c("a","d","f"), c("b", "e", "g", "z"))
#' @export
intercalate = function(..., fillin = FALSE) {
    isNested <- function(x) {
        if (class(x) != "list") {
            stop("Expecting 'x' to be a list")
        }
        out <- any(sapply(x, is.list))
        return(out)
    }
    args = list(...)
    if (isNested(args)) {
        args = unlist(args, recursive = F)
    }
    if (fillin) {
      mx = max(lengths(args))
      args = lapply(args, function(x) x[rep_len(seq_along(x), mx)])
    }
    s_along = lapply(args, seq_along)
    ord = order(do.call(c, s_along))
    conc = do.call(c, args)
    return(conc[ord])
}


#' @name matrify
#' @title take a data.table/frame, shave first column into rownames, make a matrix
#'
#' @description
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


#' @name intercalate_lst
#' @title collate lists together
#'
#' @description
#' interleave multiple vectors
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
#' @title subset out indices
#'
#' @description
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

#' @name false2na
#' @title replace FALSE with NA
#'
#' @description
#' A convenience function to set a logical vector with NAs to false
#'
#' @return A logical vector with NAs set to FALSE
#' @export
false2na = function(x) {
    if (is.logical(x))
        x[x %in% FALSE] = NA
    else
        stop("x is not logical")
    x
}

#' @name nonzero2na
#' @title replace 0 to with NA
#'
#' @description
#' A convenience function to set a logical vector with NAs to false
#'
#' @return A logical vector with NAs set to FALSE
#' @export
nonzero2na = function(x) {
    if (is.integer(x))
        naval = NA_integer_
    else if (is.double(x))
        naval = NA_real_
    else
        stop("x is not double or integer")
    x[x > 0] = naval
    x
}

#' @name na2false
#' @title replace logical vector with NA to FALSE
#'
#' @description
#' A convenience function to set a logical vector with NAs to false
#'
#' @return A logical vector with NAs set to FALSE
#' @export
na2false = function(v)
{
    ## v = ifelse(is.na(v), v, FALSE)
    ## v[is.na(v)] = FALSE
    v[isNA(v)] = FALSE
    ## mode(v) = "logical"
    v
}


#' @name na2true
#' @title replace logical vector with NA to TRUE
#'
#' @description
#' A convenience function to set a logical vector with NAs to TRUE
#'
#' @return A logical vector with NAs set to TRUE
#' @export
na2true = function(v)
{
    ## v = ifelse(is.na(v), v, FALSE)
    ## v[is.na(v)] = TRUE
    v[isNA(v)] = TRUE
    ## as.logical(v)
    ## mode(v) = "logical"
    v
}

#' @name na2zero
#' @title na2zero
#'
#' A convenience function to set a numeric vector with NAs to zero
#'
#' @return A numeric vector with NAs set to zero
#' @export
na2zero = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    ## v[is.na(v)] = 0
    v[isNA(v)] = 0
    return(v)
}

#' @name nan2zero
#' @title nan2zero
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
#' @title na2empty
#'
#' A convenience function to set a character vector with NAs to an
#' empty character
#'
#' @return A character vector
#' @export
na2empty = function(v) {
    ## v = ifelse(is.na(v), v, FALSE)
    ## v[is.na(v)] = ""
    v[isNA(v)] = ""
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
#' @title lst.empty
#'
#' A logical vector to select which list elements are empty
#'
#' @return A list
#' @export lst.empty
lst.empty = function(x) {
    lengths(x) == 0
    ## S4Vectors::elementNROWS(x) == 0
}

#' @name lst.empty2zero
#'
#' set empty list elements to zero
#'
#' @return A logical vector of length(x)
#' @export lst.empty2zero
lst.empty2zero = function(x) {
    x[lengths(x) == 0] = 0
    ## x[S4Vectors::elementNROWS(x) == 0] = 0
    x
}


#' @name lst.empty2na
#' @title empty list elements set to NA
#'
#' set empty list elements to NA
#'
#' @return A list
#' @export lst.empty2na
lst.empty2na = function(x) {
    x[lengths(x) == 0] = NA
    ## x[S4Vectors::elementNROWS(x) == 0] = NA
    ## x[x == "character(0)"] = NA
    ## x[x == "numeric(0)"] = NA
    ## x[x == "logical(0)"] = NA
    ## x[x == "integer(0)"] = NA
    x
}

#' @name lst.empty2null
#' @title empty list elements set to NULL
#'
#' set empty list elements to NULL
#'
#' @return A list
#' @export lst.empty2null
lst.empty2null = function(x) {
    x[lengths(x) == 0] = NULL
    ## x[S4Vectors::elementNROWS(x) == 0] = NULL
    ## x[x == "character(0)"] = NULL
    ## x[x == "numeric(0)"] = NULL
    ## x[x == "logical(0)"] = NULL
    ## x[x == "integer(0)"] = NULL
    x
}


#' @name lst.null2na
#' @title NULL elements of list to NA
#'
#' set NULL list elements to NA
#'
#' @return A list
#' @export lst.null2na
lst.null2na = function(x) {
    x[x == "NULL"] = NA
    x
}

#' @name lst.emptychar2null
#' @title empty character elements of list to NULL
#'
#' set empty character to null
#'
#' @return A list
#' @export lst.emptychar2null
lst.emptychar2null = function(x) {
    x[!nzchar(x)] = NULL
    x
}


#' @name lst.emptychar2na
#' @title empty character elements of list to NA
#'
#' set empty character to NA
#'
#' @return A list
#' @export lst.emptychar2na
lst.emptychar2na = function(x) {
    x[!nzchar(x)] = NA_character_
    x
}


#' @name lst.zerochar2empty
#' @title zero length character elements to empty char
#'
#' set 0 length chracter to empty
#'
#' @return A list
#' @export lst.zerochar2empty
lst.zerochar2empty = function(x) {
    x[x == "character(0)"] = list("")
    x
}

#' @name lst.empty2replace
#' @title length 0 elements of list to replace with a value
#'
#' set empty elements to a replacement value
#'
#' @return A list
#' @export lst.emptyreplace
lst.emptyreplace = function(x, replace = NA) {
    x[lengths(x) == 0] = replace
    x
}


################################################## general R utilities
##################################################
##################################################

#' @name bool
#' @title Clean Up Boolean Logic
#'
#' @description
#' wrapping around boolean statements to ignore NULL or length(0) vectors
#' in a series of boolean statements
#'
#' @export
bool = function(x, nullignore = TRUE, na2false = TRUE) {
    x1 = bools = substitute(x)
    bools = as.list(bools)
    arg1 = logical(0)
    arg2 = logical(0)
    ## lst = list()
    boollist = list()
    counter = 1
    if (!toString(bools[[1]]) %in% c("&", "|")) return(eval(x1))
    while(length(bools) > 1 && toString(bools[[1]]) %in% c("&", "|")) {
        boollist[[counter]] = eval(bools[[1]])
        ## lst[[1]] = eval(bools[[length(bools)]])
        if (counter > 1) {
            arg2 = eval(bools[[length(bools)]])
            if (anyNA(arg2) && na2false) {
                arg2 = na2false(arg2)
            }
            if (length(arg2) == 0 && length(arg1) && nullignore) {
                arg2 = arg1
            }
        } else {
            arg1 = eval(bools[[length(bools)]])
            if (anyNA(arg2) && na2false) {
                arg1 = na2false(arg1)
            }
            if (length(arg1) == 0 & nullignore) {
                arg1 = logical(0)
            }
        }

        bools = bools[-length(bools)]
        tmp = as.list(bools[[length(bools)]])
        if (length(tmp) > 1 && toString(tmp[[1]]) %in% c("&", "|"))
            bools = tmp
        if (counter > 1 && length(arg1) && length(arg2)) {
            arg1 = boollist[[counter - 1]](arg1, arg2)
        } else if (counter > 1 && length(arg1) == 0) {
            arg1 = arg2
        }
        counter = counter + 1
    }
    return(arg1)
}


#' @name copydt
#' @title copy data frame/table columns to a new data table with forced column structure
#'
#' @description
#' Ensure that all columns in out data table possess the specified columns
#' in which default values for missing columns will be NA valuess
#'
#' @export
copydt = function(dt, columns, as.data.table = TRUE) {
    out = data.frame()[seq_len(max(NROW(dt), 1)),]
    ix = seq_len(NROW(columns))
    outname = names(columns)
    badnames = !nzchar(outname) | is.na(outname)
    if (is.null(outname)) outname = columns
    if (any(badnames)) outname[badnames] = columns[badnames]
    for (i in ix) {
        cn = columns[i]
        nm = outname[i]
        if (is.null(dt[[cn]]))
            out[[nm]] = NA
        else
            out[[nm]] = rep_len(dt[[cn]], NROW(out))
    }
    if (NROW(dt) == 0) {
        out = out[0,,drop=F]
    }
    if (as.data.table) {
        setDT(out)
    }
    return(out)
}

#' @name AND
#' @title test boolean AND across multiple vectors
#'
#' @description
#'
#' @export
AND = function(FUN = identity, ...) {
    lst = lapply(list(...), FUN)
    Reduce(function(x,y) {x & y}, lst)
}

#' @name OR
#' @title test boolean OR across multiple vectors
#'
#' @description
#'
#' @export
OR = function(FUN = identity, ...) {
    lst = lapply(list(...), FUN)
    Reduce(function(x,y) {x | y}, lst)
}

#' @name dtapply
#' @title mclapply on a table split by a column
#'
#' @description
#'
#' @export
dtapply = function (tbl,  split_col = "system_id", FUN, mc.cores = 1, mc.strict = TRUE, split_col_sort = FALSE, mclapply = parallel::mclapply, ...) 
{
    ## spl = tbl[[split_col]]
    ## dups = logical(NROW(tbl))
    dups = 0
    for (x in split_col) {
        dups = dups + anyDuplicated(tbl[[x]])
    }
    if (dups > 0) {
        if (isTRUE(mc.strict)) errfun = stop else errfun = warning
        errfun("split column contains duplicates - some entries will have multiple paths")
    }
    ## if (!isTRUE(split_col_sort)) {
    ##     spl = factor(spl, unique(spl))
    ## }
    ## lst = split(tbl, spl)
    lst = split_by(tbl, split_col, split_col_sort = split_col_sort)
    mclapply(lst, mc.cores = mc.cores, FUN, ...)
}


#' @name read.header
#' @title read header of file
#'
#' @description
#'
#' @export read.header
read.header = function(path, header.char = "#") {
    n = 0
    f = file(path, open = "r")
    on.exit(close(f))
    out = character(0)
    rl = readLines(f, n = 1)
    while(grepl(paste0("^", header.char), rl)) {
        n = n + 1
        out = c(out, rl)
        rl = readLines(f, n = 1)
    }
    return(list(output = out, nlines = n))
}


#' @name kpdf
#' @title open pdf device with ppdf defaults without closing
#'
#' @description
#'
#' @export
kpdf = function(filename = "plot.pdf", height = 10, width = 10, 
    h = height, w = width, cex = 1, title = NULL, byrow = TRUE, 
    dim = NULL, cex.title = 1, oma.scale = 0, oma.val = c(1, 1, 1, 1), useDingbats = FALSE, res = 0, pars = list(), 
    ...) {
    this.env = environment()
    if (length(cex) == 1) 
        cex = rep(cex, 2)
    height = h
    width = w
    height = cex[1] * height
    width = cex[2] * width
    DEFAULT.OUTDIR = Sys.getenv("PPDF.DIR")
    if (nchar(DEFAULT.OUTDIR) == 0) 
        DEFAULT.OUTDIR = normalizePath("~/public_html/")
    if (!grepl("^[~/]", filename)) 
        filename = paste(DEFAULT.OUTDIR, filename, sep = "/")
    if (!file.exists(file.dir(filename))) 
        system(paste("mkdir -p", file.dir(filename)))
    cat("rendering to", filename, "\n")
    pdf(file = filename, height = height, width = width, useDingbats = useDingbats, ...)
    if (!is.null(dim)) {
            if (length(dim) == 1) 
                dim = rep(dim, 2)
            dim = dim[1:2]
            graphics::layout(matrix(1:prod(dim), nrow = dim[1], 
                ncol = dim[2], byrow = byrow))
    }
    if (!is.null(title)) 
        title(title, cex.main = cex.title * max(cex))
}

#' @name ksvg
#' @title open svg device with ppdf defaults without closing
#'
#' @description
#'
#' @export
ksvg = function(filename = "plot.svg", height = 10, width = 10,
                h = height, w = width, cex = 1, title = NULL, byrow = TRUE, 
                dim = NULL, cex.title = 1, oma.scale = 0, units = 'in', oma.val = c(1, 1, 1, 1),
                useDingbats = FALSE, res = 300, pars = list(),
                ...) {
    this.env = environment()
    if (length(cex) == 1) 
        cex = rep(cex, 2)
    height = h
    width = w
    height = cex[1] * height
    width = cex[2] * width
    DEFAULT.OUTDIR = Sys.getenv("PPDF.DIR")
    if (nchar(DEFAULT.OUTDIR) == 0) 
        DEFAULT.OUTDIR = normalizePath("~/public_html/")
    if (!grepl("^[~/]", filename)) 
        filename = paste(DEFAULT.OUTDIR, filename, sep = "/")
    if (!file.exists(file.dir(filename))) 
        system(paste("mkdir -p", file.dir(filename)))
    cat("rendering to", filename, "\n")
    svg(file = filename, height = height, width = width, ...)
    if (!is.null(dim)) {
        if (length(dim) == 1) 
            dim = rep(dim, 2)
        dim = dim[1:2]
        graphics::layout(matrix(1:prod(dim), nrow = dim[1], 
                                ncol = dim[2], byrow = byrow))
    }
    if (!is.null(title)) 
        title(title, cex.main = cex.title * max(cex))
}
    
    


#' @name kpng
#' @title open png device with ppng defaults without closing
#'
#' @description
#'
#' @export
kpng = function(filename = "plot.png", height = 10, width = 10,
                h = height, w = width, cex = 1, title = NULL, byrow = TRUE, 
                dim = NULL, cex.title = 1, oma.scale = 0, units = 'in', oma.val = c(1, 1, 1, 1),
                useDingbats = FALSE, res = 300, pars = list(),
                ...) {
    this.env = environment()
    if (length(cex) == 1) 
        cex = rep(cex, 2)
    height = h
    width = w
    height = cex[1] * height
    width = cex[2] * width
    DEFAULT.OUTDIR = Sys.getenv("PPDF.DIR")
    if (nchar(DEFAULT.OUTDIR) == 0) 
        DEFAULT.OUTDIR = normalizePath("~/public_html/")
    if (!grepl("^[~/]", filename)) 
        filename = paste(DEFAULT.OUTDIR, filename, sep = "/")
    if (!file.exists(file.dir(filename))) 
        system(paste("mkdir -p", file.dir(filename)))
    cat("rendering to", filename, "\n")
    png(file = filename, height = height, width = width, units = units, res = res, ...)
    if (!is.null(dim)) {
        if (length(dim) == 1) 
            dim = rep(dim, 2)
        dim = dim[1:2]
        graphics::layout(matrix(1:prod(dim), nrow = dim[1], 
                                ncol = dim[2], byrow = byrow))
    }
    if (!is.null(title)) 
        title(title, cex.main = cex.title * max(cex))
}

#' @name kpng
#' @title open png device with ppng defaults without closing
#'
#' @description
#'
#' @export
kjpeg = function(filename = "plot.jpeg", height = 10, width = 10,
                h = height, w = width, cex = 1, title = NULL, byrow = TRUE, 
                dim = NULL, cex.title = 1, oma.scale = 0, units = 'in', oma.val = c(1, 1, 1, 1),
                useDingbats = FALSE, res = 300, quality = 75, pars = list(),
                ...) {
    this.env = environment()
    if (length(cex) == 1) 
        cex = rep(cex, 2)
    height = h
    width = w
    height = cex[1] * height
    width = cex[2] * width
    DEFAULT.OUTDIR = Sys.getenv("PPDF.DIR")
    if (nchar(DEFAULT.OUTDIR) == 0) 
        DEFAULT.OUTDIR = normalizePath("~/public_html/")
    if (!grepl("^[~/]", filename)) 
        filename = paste(DEFAULT.OUTDIR, filename, sep = "/")
    if (!file.exists(file.dir(filename))) 
        system(paste("mkdir -p", file.dir(filename)))
    cat("rendering to", filename, "\n")
    jpeg(file = filename, height = height, width = width, units = units, res = res, quality = quality, ...)
    if (!is.null(dim)) {
        if (length(dim) == 1) 
            dim = rep(dim, 2)
        dim = dim[1:2]
        graphics::layout(matrix(1:prod(dim), nrow = dim[1], 
                                ncol = dim[2], byrow = byrow))
    }
    if (!is.null(title)) 
        title(title, cex.main = cex.title * max(cex))
}


#' @name readin
#' @title flexible file opening
#'
#' @description
#'
#' @export
readin <- function(x, txt.fun = data.table::fread,
                   vcf.fun = skidb::read_vcf, other.txt = NULL,
                   alt.fun = NULL, alt.ext = NULL,
                   other.compress = NULL) {
    compression_ext = c("gz", "bz2", "xz")
    if (!is.null(other.compress) && is.character(other.compress)) {
        compression_ext = c(compression_ext, na.omit(other.compress))
    }
    compression.ptrn = paste0(paste0("(.", sub("^\\.", "", compression_ext), ")"), collapse = "|")
    txt_ext = c("txt", "csv", "tsv")
    if (!is.null(other.txt) && is.character(other.txt)) {
        txt_ext = c(txt_ext, na.omit(other.txt))
    }
    if (!is.null(alt.fun) && is.function(alt.fun) &&
        !is.null(alt.ext) && is.character(alt.ext)) {
        .NotYetImplemented()
    }
    vcf_ext = c("vcf")
    rds_ext = c("rds")
    txt.ptrn = paste0(paste0("(.", sub("^\\.", "", c(txt_ext, other.txt)), ")"), collapse = "|")
    txt.ptrn = paste0('(', txt.ptrn, ")(", compression.ptrn, "){0,}$")
    rds.ptrn = paste0(paste0("(.", sub("^\\.", "", rds_ext), ")"), collapse = "")
    rds.ptrn = paste0('(', rds.ptrn, ")(", compression.ptrn, "){0,}$")
    vcf.ptrn = paste0(paste0("(.", sub("^\\.", "", vcf_ext), ")"), collapse = "")
    vcf.ptrn = paste0('(', txt.ptrn, ")(", compression.ptrn, "){0,}$")
    ## is.txt = grepl("((.txt)|(.csv)|(.tsv))((.gz)|(.bz2)|(.xz)){0,}$", x, T)
    is.txt = grepl(txt.ptrn, x, T)
    is.rds = grepl(rds.ptrn, x, T)
    ## is.vcf = grepl("((.vcf))((.gz)|(.bz2)|(.xz)){0,}$", x, T)
    is.vcf = grepl(vcf.ptrn, x, T)
    if (isTRUE(is.txt))
        return(txt.fun(x))
    else if (isTRUE(is.rds))
        return(readRDS(x))
    else if (isTRUE(is.vcf))
        return(vcf.fun(x))
    else if (!file.exists(x) && !identical(x, "/dev/null")) {
        warning("File does not exist")
        return(structure(list(), msg = "File does not exist"))
    } else
        stop("file extension not recognized")
}


#' @name save.r
#' @title save R data session
#'
#' @description
#'
#' @export save.r
save.r <- function(file, note = NULL, verbose = FALSE, compress = FALSE, ...) {
    stamped.file = gsub(".RData$", paste(".", timestamp(), ".RData", 
                                         sep = ""), file, ignore.case = TRUE)
    if ( compress ) {
        message("Compression of the .RData object set to TRUE... Saving will take a while...")
    } else {
        message("Compression of the .RData object set to FALSE... Saving will be faster than with compression.")
        message("Keep an eye on disk space usage!")
    }
    save.image(stamped.file, compress = compress, ...)
    if (file.exists(file)) {
        if (verbose) 
            message("Removing existing ", file)
        system(paste("rm", file))
    }
    if (verbose) 
        message("Symlinking ", file, " to ", stamped.file)
    system(paste("ln -sfn", normalizePath(stamped.file), file))
    if (!is.null(note)) {
        writeLines(note, paste0(stamped.file, ".readme"))
    }
}

#' @name trans
#' @title transpose a list
#'
#' @description
#'
#' @export
trans <- function (lst, ffun = list) 
{
    do.call(Map, c(f = ffun, lst))
}

#' @name transp
#' @title transpose a list
#'
#' @description
#'
#' @export
transp <- trans

#' @name fitzscore
#' @title calculate zscores based on prior mean and stddev
#'
#' @description
#'
#' @export
fitzscore <- function(x, mean, stddev) {
    structure((x - mean) / stddev, mean = mean, stddev = stddev)
}

#' @name softmax
#' @title calculate softmax
#'
#' @description
#'
#' @export
softmax <- function(x, neg = FALSE) {
    if (neg) {
        numer = exp(-x)
    } else {
        numer = exp(x)
    }
    denom = sum(numer)
    return(numer / denom)
}


#' @name un
#' @title shortcut to check unique entries
#'
#' @description
#'
#' @export un
un <- function(..., i = 1, sep = " ") {
  lst = as.list(match.call())[-1]
  ix = setdiff(seq_along(lst), which(names(lst) %in% c("ix")))
  if (length(ix) > 1) 
    vec = do.call(function(...) paste(..., sep = sep), list(...))
  else vec = unlist(list(...))
  unique_ix = which(!duplicated(vec))
  which(vec %in% vec[unique_ix][i])
}

## un <- function(x, ix = 1) {
##     unique_ix = which(!duplicated(x))
##     which(x %in% x[unique_ix][ix])
## }


#' @name eNROW
#' @title does vapply NROW
#'
#' @description
#'
#' @export eNROW
eNROW <- function(x) {
    return(vapply(x, NROW, integer(1)))
}


#' @name frac
#' @title fraction from a vector of numeric values
#'
#' @description
#'
#' @export frac
frac = function(x) {
    x / sum(x)
}


#' @name row.sort
#' @title sort rows of integer matrix
#'
#' @description
#' matrix(a[order(row(a), a)], ncol = ncol(a), byrow = TRUE)
#' ^^ came from stackoverflow
#' https://stackoverflow.com/questions/9506442/fastest-way-to-sort-each-row-of-a-large-matrix-in-r
#'
#' Rfast::rowsort is faster than the base function but it is still very fast
#'
#' @export row.sort
row.sort <- function(a, use_rfast = TRUE) {
    out = tryCatch(Rfast::rowSort(a),
                   error = function(e) matrix(a[order(row(a), a)], ncol = ncol(a), byrow = TRUE))
    return(out)
}


#' @name rows.all
#' @title test whether all row entries are TRUE
#'
#' @description
#'
#' @export rows.all
rows.all <- function(mat) {
    vec = logical(NROW(mat))
    for (i in seq_len(NROW(mat))) {
        vec[i] = all(mat[i,])
    }
    return(vec)
}

#' @name rows.any
#' @title test whether any row entries are TRUE
#'
#' @description
#'
#' @export rows.any
rows.any <- function(mat) {
    vec = logical(NROW(mat))
    for (i in seq_len(NROW(mat))) {
        vec[i] = any(mat[i,])
    }
    return(vec)
}

#' @name make_dummy
#' @title make table of dummy encodings
#'
#' @description
#'
#' @export
make_dummy = function(x, field = ".", sep = ".", levelsOnly = FALSE, fullRank = FALSE, return.data.table = T) {
  paste(field, collapse = ", ")
  this.str = paste0(" ~ ", field)
  mod = caret::dummyVars(this.str, data = x, sep = sep, levelsOnly = levelsOnly,
    fullRank = fullRank)
  out = caret:::predict.dummyVars(mod, newdata = x)
  if (return.data.table)
    return(as.data.table(out))
  else
    return(out)
}


#' @name somejit
#' @title add tiny jitter
#'
#' @description
#'
#' @export
somejit <- function(x, factor = 1e-6) {
    set.seed(10); jitter(x, factor = factor)
}

#' @name jitter2
#' @title jitter with consistent seed
#'
#' @description
#'
#' @export
jitter2 = function(x, factor = 1, amount = NULL, seed = 10) {
    set_rngseed(seed)
    jitter(x, factor = factor, amount = amount)
}

#' @name matrify2
#' @title create matrix from table like obj with dplyr syntax
#'
#' @description
#'
#' @export
matrify2 = function(df, ..., rownames = 1, rnsep = " ") {
    vec = c()
    arglst = match.call(expand.dots = F)$`...`
    if (NROW(arglst)) {
        for (thisexpr in arglst) {
            vec = c(vec, deparse(substitute(thisexpr)))
        }
        expr = paste(vec, collapse = ", ")
    } else {
        expr = "-1"
    }
    if ( !inherits(df, "data.frame") && (!is.null(dim(df)) | is.list(df)) ) {
        df = as.data.frame(df)
    }
    rnexpr = deparse(substitute(rownames))
    rncols = eval(parse(text = sprintf("dplyr::select(df, %s)", rnexpr)))
    rn = dodo.call2(paste, c(as.list(rncols), sep = rnsep))
    mat = as.matrix(eval(parse(text = sprintf("dplyr::select(df, %s)", expr))))
    rownames(mat) = rn
    return(mat)
}


#' @name upset2
#' @title wrapper around UpSetR::upset()
#'
#' @description
#'
#' @export
upset2 = function(data, text.scale = 1.5, mb.ratio = c(0.7, 0.3), empty.intersections = "on", ...) {
    if (class(data)[1] != "data.frame") data = as.data.frame(data)
    coercethese = which(sapply(data, nott(inherits), c("integer")))
    for (i in coercethese) {
        data[[i]] = sign(data[[i]])
    }
    UpSetR::upset(data, order.by = "freq", text.scale = text.scale, keep.order = TRUE, sets = rev(colnames(data)), mb.ratio = c(0.6, 0.4), nsets = 1e6, empty.intersections = empty.intersections, ...)
}


#' @name sampler
#' @title sample elements of vector or rows of table
#'
#' @description
#'
#' @export
sampler = function(x, n = NROW(x), seed = 10, rngkind = "L'Ecuyer-CMRG", verbose = FALSE, replace = FALSE, prob = NULL) {
    if (NROW(seed) & !isNA(seed)) {
        current.rng = .Random.seed
        txt = parse(text = sprintf(".Random.seed = as.integer(%s)", mkst(current.rng)))
        set_rngseed(seed = seed, rngkind = rngkind, verbose = verbose)
        on.exit(eval(txt, envir = parent.frame()))
    }

    x_nrow = NROW(x)
    ix = seq_len(x_nrow)
    
    if (isFALSE(replace) && n > x_nrow) {
        warning("replace = FALSE, but n supplied is greater than dimension of input")
        warning("requested n=", n, " sampled points; ", "sampling n=", n, " instead")
        n = x_nrow
    }
    
    randid = sample(ix, n, replace = replace, prob = prob)

    if (!is.null(dim(x)))
        return(x[randid,,drop=F])
    else
        return(x[randid])
}

#' @name enframe_list
#' @title data table-ize list elements and add name column
#'
#' @description
#'
#' @export
enframe_list = function(lst, name = "name", value = "value", as.data.table = TRUE, rbind = TRUE, mc.cores = 1) {
    nms = names(lst)
    expr = parse(text = sprintf("cbind(%s = nm, df)", name, value))
    out = mcmapply(function(el, nm) {
        if (NROW(el)) {
            if (is.null(dim(el)))
                df = setColnames(as.data.frame(el), value)
            else
                df = as.data.frame(el)
            if (as.data.table)
                setDT(df)
            eval(expr)
        }
    }, lst, nms, SIMPLIFY = FALSE, mc.cores = mc.cores)
    if (rbind) {
        if (as.data.table)
            return(rbindlist(out))
        else
            return(do.call(rbind, out))
    }
}



#' @name isNA
#' @title is.na but also tests for "NA" character
#'
#' @description
#'
#' @export
isNA = function(x, na.char = c("NA", "NULL", "na", "null")) {
    if (is.character(x)) {
        return(is.na(x) | x %in% na.char)
    } else {
        return(is.na(x))
    }
}


#' @name uncut
#' @title get upper and lower bounds of cut labels
#'
#' @description
#' grab the upper and lower bounds from the default factor output
#' of cut()
#'
#' @export
uncut = function(v) {
    lst = lapply(tstrsplit(levels(v), ", "), function(x) as.numeric(gsub("[][)]+", "", x)))
    names(lst) = c("lb", "ub")
    return(lst)
}

#' @name nodim
#' @title testing if object is empty
#'
#' @description
#' 
#'
#' @export
nodim = function(x) {
    any(DIM(x) == 0)
}

#' @name NCOL2
#' @title extending NCOL
#'
#' @description
#' NCOL = 1 for NULL, or any vector with length == 0
#' seems counterintuitive so this is the fix
#' 
#'
#' @export
NCOL2 <- function(x) {
  d = dim(x)
  ln = length(d)
  lx = length(x)
  if (ln > 1L) {
    d[2L]
  } else if (lx == 0L) {
    0L
  } else {
    1L
  }
}

#' @name DIM
#' @title extending NROW and NCOL
#'
#' @description
#' 
#'
#' @export
DIM = function(x) {
    return(c(NROW(x), NCOL(x)))
}

#' @name DIM2
#' @title extending NROW and NCOL2
#'
#' @description
#' 
#'
#' @export
DIM2 <- function(x) {
    return(c(NROW(x), NCOL2(x)))
}


#' @name w.quantile
#' @title weighted quantile
#'
#' @description
#' 
#'
#' @export w.quantile
w.quantile = function(x, w = 1, qs) {
    ix = order(x)
    if (length(x) != length(x)) 
        w = rep_len(w, NROW(x))
    tot = sum(w)
    x[ix]
    cs = cumsum(w[ix])
    selectix = sapply(qs, function(thisx) which.max(cumsum(cs <= (tot * thisx))))
    return(x[ix][selectix])
}


#' @name metanames
#' @title metanames
#'
#' @description
#' 
#'
#' @export
metanames = function(x) {
    if (inherits(x, c("GRanges", "GRangesList")) || "elementMetadata" %in% slotNames(x)) {
        x = mcols(x)
    }
    nm = colnames(x)
    if (is.null(nm)) {
        return(rep_len("", NCOL(x)))
    } else {
        return(nm)
    }
}

#' @name split_by
#' @title split data frame-like object based on columns
#'
#' @description
#' 
#'
#' @export
split_by = function(dt, fields, do.unname = FALSE, split_col_sort = FALSE) {
    in_type_is_granges = inherits(dt, c("GRanges", "IRanges", "GRangesList", "IRangesList"))
    if (in_type_is_granges) {
        out = dt
        dt = mcols(dt)
        si = seqinfo(out)
        if (!NROW(out))
            return(gr.fix(GRangesList(), si))
    }
    if (!NROW(dt)) return(list())
    colix = match3(fields, names(dt))
    ## colix = which(names(dt) %in% fields)
    expr = parse(text = sprintf("dt[,%s,drop=FALSE]", mkst(colix)))
    cols = eval(expr)
    if (!isTRUE(split_col_sort)) 
        uf = dodo.call2(FUN = function(...) uniqf(..., sep = " "), as.list(cols))
    else
        uf = dodo.call2(FUN = function(...) paste(..., sep = " "), as.list(cols))
    ## rles = dodo.call2(FUN = rleseq, as.list(cols))
    if (in_type_is_granges) {
        out = split(out, uf)
        mcols(out) = dt[!duplicated(uf), fields]
        if (do.unname) out = unname(out)
        return(out)
    }
    out = split(dt, uf)
    if (do.unname) out = unname(out)
    return(out)
}


#' @name printerr
#' @title print error message from tryCatch
#'
#' @description
#' useful for tryCatch(..., error = function(e) printerr(<some_custom_msg>))
#'
#' @export
printerr = function(msg = "", e) {
    if (missing(e)) {
        e = dg(e)
    }
    cm = as.character(conditionMessage(e))
    cc = as.character(conditionCall(e))
    eval(quote(print(structure(paste("error: ", msg, cm, cc), class = "err"))))
}

#' @name nott
#' @title nott
#'
#' @description
#' 
#'
#' @export
nott = function(f) {
    if (missing(f) || is.null(f) || !is.function(f)) f = identity
    return(Negate(f))
}

#' @name fillby
#' @title fill in variables of data table by combos
#'
#' @description
#' especially useful for expanding factors excluded from a query
#'
#' @export
fillby = function(x, by, fillcol, fill = 0L, use_factor_levels = TRUE) {
    strby = paste0(by, "=", "unique(", by, ")")
    fc = which(sapply(x[, by, with = FALSE], inherits, "factor"))
    if (use_factor_levels && any(fc)) strby[fc] = paste0(by[fc], "=", "levels(", by[fc],")")
    strby = paste(strby, collapse = ",")
    cj = et(sprintf("x[, CJ(%s)]", strby))
    out = suppressWarnings(setkeyv(copy3(x), by)[cj])
    fill = rep_len(fill, length(fillcol))
    for (i in seq_along(fillcol)) {
        data.table::set(out, j = fillcol[i], value = replace_na(out[[fillcol[i]]], fill))
    }
    return(out)
}


#' @name seevar
#' @title see variables in environment
#'
#' @description
#' seevar
#'
#' @export
seevar = function(calling_env = parent.frame()) {
    setdiff(ls(envir = calling_env), lsf.str(envir = calling_env))
}


#' @name reset.dev
#' @title reset.dev
#'
#' @description
#' dealing with annoying plot resets
#'
#' @export reset.dev
reset.dev = function(x) {
  err = NULL
  while (is.null(err)) {
    err = tryCatch({eval(quote(dev.off()), globalenv()); NULL}, error = function(e) structure("", class = "err"))
    ## err = tryCatch({evalq(dev.off(), globalenv()); NULL}, error = function(e) structure("", class = "err"))
  }
}



#' @name complete.cases2
#' @title complete.cases wrapper
#'
#' @description
#' can use this within data.table
#'
#' @export complete.cases2
complete.cases2 = function(...) {
    complete.cases(as.data.frame(list(...)))
}

#' @name uniqf
#' @title Unique factor
#'
#' @description
#' Make a unique factor based on one or more vectors
#' in parallel.
#'
#' @export uniqf
uniqf = function (..., sep = " ")
{
    set.seed(10)
    lst = as.list(match.call()[-1])
    force(sep)
    nm = names(lst)
    if (is.null(nm)) {
        nm = rep_len("", length(lst))
    }
    ix = which(nm != "sep")
    tmpix = do.call(paste, c(lst[ix], alist(sep = sep)))
    tmpix = factor(tmpix, levels = unique(tmpix))
    tmpix
}


## uniqf = function(..., sep = paste0(" ", rand.string(length = 8), 
##                                    " ")) {
##     current.rng = .Random.seed
##     txt = parse(text = sprintf(".Random.seed = as.integer(%s)", mkst(current.rng)))
##     set_rngseed(seed = 10, verbose = FALSE)
##     on.exit(eval(txt, envir = globalenv()))
##     lst = as.list(match.call()[-1])
##     ix = which(names2(lst) != "sep")
##     ## senv = suppressWarnings(stackenv(parent.frame()))
##     tmpix = do.call(paste, c(lst[ix], alist(sep = dg(sep))), envir = parent.frame())
##     tmpix = factor(tmpix, levels = unique(tmpix))
##     tmpix
## }

#' @name qq
#' @title get actual quantile values of vector
#'
#' @description
#' Use the empirical distribution of numeric values
#' to get the quantiles assigned to each value of a vector
#'
#' @export qq
qq = function(x) {
    ecdf(x)(x)
}

#' @name qtrim
#' @title cut off vector by quantiles
#'
#' @description
#' Use the empirical distribution of numeric values
#' to get the quantiles assigned to each value of a vector.
#' then remove values that are above the quantile cutoff.
#'
#' @export qtrim
qtrim = function(x, maxq = 0.99) {
    x[qq(x) < maxq]
}

#' @name tailf
#' @title modification of tailf from skitools
#'
#'
#' @export tailf
tailf = function (x, n = NULL, grep = NULL) {
    oldscipen = options()$scipen
    tmp = tempfile()
    on.exit({options(scipen = oldscipen); unlink(tmp)})
    options(scipen = 999)
    if (is.null(grep)) {
        if (is.null(n)) {
            x = paste("tail -F", paste(x, collapse = " "))
        }
        else {
            x = paste("tail -n", n, "-F", paste(x, collapse = " "))
        }
    }
    else {
        x = paste("grep -H", grep, paste(x, collapse = " "),
            " | more")
    }
    writeLines(x, tmp)
    system2("sh", tmp)
}

#' @name ne.na
#' @title mark x with NA if it does not exist
#'
#'
#' @export
ne.na = function(x, no = NA_character_) {
    ifelse(file.not.exists(x), no, x)
}


#' @name flag2int
#' @title convert bam flag to integer
#'
#'
#' @export
flag2int = function(flags) {
    apply(flags, 1, function(x) sum(2^((1:12) - 1) * x))
}


#' @name mkst
#' @title MaKe STring
#'
#' making string out of vector for eval(parse(text = ...))
#'
#'
#' @export
mkst = function(v, f = "c", po = "(", pc = ")", collapse = ",", asnull = FALSE) {
    if (identical(NROW(v), 0L)) {
        if (isTRUE(asnull)) return(NULL) else return("")
    }
    out = paste0(f, po, paste0(v, collapse = collapse), pc)
    return(out)
}


#' @name unI
#' @title remove "AsIs" from class, i.e. undo I(obj)
#'
#' undo I(obj)
#'
#'
#' @export
unI = function(x) {
  if (inherits(x, "AsIs")) class(x) = setdiff(class(x), "AsIs")
  return(x)
}


#' @name log10p
#' @title log10(x + 1)
#'
#'
#' @export
log10p = function(x) {
    log10(x + 1)
}


#' @name .gc
#' @title .gc
#'
#'
#'
#' @param df data frame
#' @param ptrn pattern
#' @author Kevin Hadi
#' @export
.gc = function(df, ptrn, invert = F, ignore.case = FALSE, exact = FALSE) {
    if (inherits(df, c("GRanges", "GRangesList")))
        cnames = colnames2(df@elementMetadata)
    else
        cnames = colnames2(df)
    if (is.character(ptrn)) {
        if (!exact) {
            ## ptrn = paste0("^(", ptrn, ")$")
            ix = loop_grep(ptrn, cnames, ignore.case = ignore.case)
        } else {
            ix = which(cnames %in% ptrn)
        }
    } else {
        ix = ptrn
    }
    if (NROW(ix)) {
        if (isTRUE(invert)) inv = "-" else inv = ""
        return(et(sprintf("df[,%s%s,drop=F]", inv,
                          paste0("c(", paste(ix, collapse = ","), ")"))))
    } else {
        if (isTRUE(invert))
            return(df)
        else
            return(df[,0,drop=F])
    }
}

#' @name rmcol
#' @title rmcol
#'
#'
#'
#' @param df object with !is.null(df) == TRUE
#' @author Kevin Hadi
#' @export rmcol
rmcol = function(df, rmcol = 1, usekey = T, invert = T, exact = FALSE) {
    if (is.data.table(df) && !is.null(key(df)) && usekey) {
        rmcol = key(df)
        exact = TRUE
        invert = TRUE
    }
    return(.gc(df, rmcol, invert = invert, exact = exact))
}

#' @name do.cols
#' @title do.cols
#'
#'
#'
#' @param x object with ncol >= 1
#' @author Kevin Hadi
#' @export do.cols
do.cols = function(x, rmcol = 1, FUN = rowSums, by.FUN = NULL, exact = F, invert = T) {
    if (NROW(by.FUN)) {
        if (names(as.list(args(by.FUN))[1]) == "...") {
            return(as.data.table(.gc(x, rmcol, invert = invert, exact = exact))[, I := .I][, do.call(by.FUN, .SD), by = I]$V1)
        } else {
            return(as.data.table(.gc(x, rmcol, invert = invert, exact = exact))[, I := .I][, by.FUN(.SD), by = I]$V1)
        }
    } else if (NROW(FUN)) {
        FUN(.gc(x, rmcol, invert = invert, exact = exact))
    } else {
        return(x)
    }
}


#' @name parasn
#' @title assign parallel columns
#'
#'
#'
#' @param x data.table
#' @param y data.table
#' @author Kevin Hadi
#' @export
parasn = function(x, y, cols, sans_key = TRUE, use.data.table = T) {
    if (missing(cols)) {
        if (sans_key)
            cols = setdiff(colnames(y), key(x))
        else
            cols = colnames(y)
    }
    dimx = DIM(x)
    dimy = DIM(y)
    if (dimx[1] != dimy[1]) {
        nr.x = dimx[1]
        id.x = seq_len(nr.x)
        nr.y = dimy[1]
        id.y = seq_len(nr.y)
        id.x = rep_len(nr.x, id.x)
        id.y = rep_len(nr.y, id.y)
        x = x[id.x]
        y = y[id.y]
    }
    if (use.data.table) {
        for (col in cols) {
            set(x, j = col, value = y[[col]])
        }
    } else {
        for (col in cols) {
            x[[col]] = y[[col]]
        }
    }
    return(x)
}


#' @name do.assign
#' @title assign columns or list elements
#'
#'
#'
#' @author Kevin Hadi
#' @export do.assign
do.assign = function(x, ..., pf = parent.frame()) {
  mc_2340873450987 = match.call(expand.dots = FALSE)
  ddd = as.list(mc_2340873450987)$`...`
  if (is.null(names(ddd))) names(ddd) = paste0(rep_len("V", length(ddd)), seq_along(ddd))
  for (i in seq_along(ddd)) {
    d = ddd[[i]]
    nml = names(ddd[i])
    if (is.call(d) || is.name(d)) {
      ev = BiocGenerics::eval(d, envir = parent.frame())
      nm = names(ev)
      .DIM = DIM2(ev)
      .dim = dim(ev)
      nr = .DIM[1L]
      nc = .DIM[2L]
      if (inherits(ev, c("list")) && nc == 1L) {
        if (is.null(nm)) nm = rep_len(nml, nr)
        for (ii in seq_len(nr)) {
          x[[nm[ii]]] = ev[[ii]]
        }
      } else if (length(.dim) > 0L) {
        if (is.null(nm)) nm = rep_len(nml, nc)
        for (ii in seq_len(nc)) {
          x[[nm[ii]]] = ev[[ii]]
        }
      } else {
        x[[nml]] = ev
      }
    }
  }
  return(x)
}


#' @name duped
#' @title duped
#'
#'
#'
#' @param ... vectors to paste by
#' @author Kevin Hadi
#' @export
duped = function(..., binder = "data.table") {
    duplicated(tryCatch(et(sprintf("%s(...)", binder)),
                        ## error = function(e) do.call(cbind, list(...))))
                        error = function(e) paste(...)))
}


#' @name colexists
#' @title find out if column is in data.table
#'
#'
#'
#' @param nm name of column
#' @param df data.frame or data.table
#' @author Kevin Hadi
#' @export colexists
colexists = function(nm, df) {
    cnames = colnames2(df)
    return(nm %in% cnames)
}

#' @name dodo.call2
#' @title dodo.call+
#'
#' FUN can be an anonymous function call
#' dodo.call2({function(...) paste(..., collapse = " ")}, list(bstring1, bstring2))
#' can also omit the brackets
#'
#'
#'
#' @param FUN function
#' @author Marcin Imielinski
#' @export dodo.call2
dodo.call2 = function (FUN, args, use.names = T)
{
    if (!is.character(FUN))
      FUN = substitute(FUN)
    if (isTRUE(use.names) && !is.null(names(args)))
      argn = paste0("\"", names(args), "\"", "=")
    else
        argn = NULL
    if (!is.matrix(args)) {
        cmd = paste(
            paste0("{", as.character(as.expression(FUN)), "}"),
            "(",
            paste0(argn, "args[[", 1:length(args), "]]", collapse = ","),
            ")",
            sep = "")
    } else {
        cmd = paste(
            paste0("{", as.character(as.expression(FUN)), "}"),
            "(",
            paste0(argn, "args[,", 1:ncol(args), "]", collapse = ","),
            ")",
            sep = "")
    }
    return(et(cmd))
}

#' @name nonacol
#' @title no columns named "NA"
#'
#'
#' @param x data frame
#' @author Kevin Hadi
#' @export nonacol
nonacol = function(x, napattern = "^NA") {
  good = grep(napattern, colnames(x), invert = T)
  return(et(sprintf("x[, %s,drop=F]", mkst(good))))
}



#' @name et
#' @title shortcut for eval(parse(text = <string>))
#'
#' @param txt string to evaluate
#' @author Kevin Hadi
#' @export
et = function(txt, eval = TRUE, envir = parent.frame(), enclos = parent.frame(2)) {
    out = parse(text = txt)
    ## enclos = stackenv2()
    if (eval) {
        return(eval(out, envir = envir, enclos = enclos))
    } else {
        return(out)
    }
}



#' @name clobber
#' @title same as dplyr::coalesce
#'
#' clobber NA, or some value between multiple vectors
#' bads can be a function that returns a logical
#'
#'
#' @param ... vectors to merge together
#' @param bads a set of values to clobber, or a function that returns a logical
#' @param r2l merge from left to right per pair of vectors
#' @param fromLast if TRUE, merge from last vector to first
#' @param comparefun A 2 argument function (i.e. function(x,y) x < y), if r2l = FALSE, then the greater value will be chosen as y is on the right, for function(x,y) x < y. if r2l = TRUE, then the lesser value will be chosen
#' @export
clobber = function(..., bads = NA, bads.x = NULL, bads.y = NULL, r2l = FALSE, fromLast = FALSE, opposite = TRUE, comparefun = NULL, remove.empty = TRUE) {
    lst = list(...)
    lens = eNROW(lst)
    maxlen = max(lens)
    if (length(unique(lens)) > 1)
        lst = lapply(lst, function(x) rep(x, length.out = maxlen))
    if (remove.empty)
        lst = lst[eNROW(lst) > 0]
    if ( !length(bads) && !length(bads.x) && !length(bads.y))
        stop("You gotta set one of bads, bads.x, or bads.y")
    if ({ length(bads.x) && length(bads.y) }) {
        message("bads.x and bads.y both set explicitly")
        message("setting opposite to FALSE")
        opposite = FALSE
    }
    anytrue = function(vec) rep(TRUE, length.out = length(vec))
    if (isTRUE(bads) || !length(bads)) {
        message("bads set to NULL or TRUE")
        message("setting opposite to FALSE")
        bads = anytrue
        opposite = FALSE
    }
    if (opposite) {
        yfun = get("!", mode = "function")
    } else {
        yfun = get("identity", mode = "function")
    }
    if (!length(bads.x)) bads.x = bads
    if (!length(bads.y)) bads.y = bads
    dofun = function(x,y) {
        if (is.function(bads.x))
            badsx = which(bads.x(x))
        else
            badsx = which(x %in% bads.x)
        if (is.function(bads.y))
            nbadsy = which(yfun(bads.y(y)))
        else
            nbadsy = which(yfun(y %in% bads.y))
        ix = intersect(badsx, nbadsy)
        return(replace(x, ix, rep(y[ix], length.out = length(ix))))
    }
    if (is.null(comparefun)) {
        if (!r2l)
            return(Reduce(function(x,y) dofun(x,y), lst, right = fromLast))
        else
            return(Reduce(function(x,y) dofun(y,x), lst, right = fromLast))
    } else {
        yfun = get("identity", mode = "function")
        if (!r2l) {
            return(Reduce(function(x,y) {
                if (is.function(bads.x))
                    badsx = which(bads.x(x))
                else
                    badsx = which(x %in% bads.x)
                if (is.function(bads.y))
                    nbadsy = which(yfun(bads.y(y)))
                else
                    nbadsy = which(yfun(y %in% bads.y))
                lg = which(comparefun(x,y))
                lg = setdiff(lg, nbadsy)
                out = x
                out[badsx] = y[badsx]
                out[lg] = y[lg]
                out
            }, lst, right = fromLast))
        } else {
            return(Reduce(function(x,y) {
                if (is.function(bads.x))
                    badsx = which(bads.x(x))
                else
                    badsx = which(x %in% bads.x)
                if (is.function(bads.y))
                    nbadsy = which(yfun(bads.y(y)))
                else
                    nbadsy = which(yfun(y %in% bads.y))
                lg = which(comparefun(x,y))
                lg = setdiff(lg, nbadsy)
                out = y
                out[nbadsy] = x[nbadsy]
                out[lg] = x[lg]
                out
            }, lst, right = fromLast))
        }
    }
}

## clobber = function(..., bads = NA, bads.x = NULL, bads.y = NULL, r2l = FALSE, fromLast = FALSE, opposite = TRUE) {
##     lst = list(...)
##     lens = lengths(lst)
##     maxlen = max(lens)
##     if (length(unique(lens)) > 1)
##         lst = lapply(lst, function(x) rep(x, length.out = maxlen))
##     if ( !length(bads) && !length(bads.x) && !length(bads.y))
##         stop("You gotta set one of bads, bads.x, or bads.y")
##     if ({ length(bads.x) && length(bads.y) }) {
##         message("bads.x and bads.y both set explicitly")
##         message("setting opposite to FALSE")
##         opposite = FALSE
##     }
##     anytrue = function(vec) rep(TRUE, length.out = length(vec))
##     if (isTRUE(bads) || !length(bads)) {
##         message("bads set to NULL or TRUE")
##         message("setting opposite to FALSE")
##         bads = anytrue
##         opposite = FALSE
##     }
##     if (opposite) {
##         yfun = get("!", mode = "function")
##     } else {
##         yfun = get("identity", mode = "function")
##     }
##     if (!length(bads.x)) bads.x = bads
##     if (!length(bads.y)) bads.y = bads
##     dofun = function(x,y) {
##         if (is.function(bads.x))
##             badsx = which(bads.x(x))
##         else
##             badsx = which(x %in% bads.x)
##         if (is.function(bads.y))
##             nbadsy = which(yfun(bads.y(y)))
##         else
##             nbadsy = which(yfun(y %in% bads.y))
##         ix = intersect(badsx, nbadsy)
##         return(replace(x, ix, rep(y[ix], length.out = length(ix))))
##     }
##     if (!r2l)
##         return(Reduce(function(x,y) dofun(x,y), lst, right = fromLast))
##     else
##         return(Reduce(function(x,y) dofun(y,x), lst, right = fromLast))
## }

## clobber = function (..., bads = NA, r2l = FALSE, fromLast = FALSE, opposite = TRUE)
## {
##     lst = list(...)
##     lens = lengths(lst)
##     maxlen = max(lens)
##     if (length(unique(lens)) > 1)
##         lst = lapply(lst, function(x) rep_len(x, maxlen))
##     if (opposite) {
##         yfun = get("!", mode = "function")
##     }
##     else {
##         yfun = get("identity", mode = "function")
##     }
##     dofun = function(x, y) {
##         if (is.function(bads)) {
##             badsx = which(bads(x))
##             nbadsy = which(yfun(bads(y)))
##         }
##         else {
##             badsx = which(x %in% bads)
##             nbadsy = which(yfun(y %in% bads))
##         }
##         ix = intersect(badsx, nbadsy)
##         return(replace(x, ix, rep_len(y[ix], length(ix))))
##     }
##     if (!r2l)
##         return(Reduce(function(x, y) dofun(x, y), lst, right = fromLast))
##     else return(Reduce(function(x, y) dofun(y, x), lst, right = fromLast))
## }

#' @name coalesce
#' @title same as dplyr::coalesce, khtools:coalesce is an alias for khtools::clobber
#'
#' clobber NA, or some value between multiple vectors
#' bads can be a function that returns a logical
#'
#' @export
coalesce = clobber

#' @name enframe
#' @title same as tibble::enframe
#'
#' enframe
#'
#'
#' @param x named vector
#' @param name name of column containing names(x)
#' @param value name of column containing x values
#' @export
enframe = function(x, name = "name", value = "value", as.data.table = TRUE) {
    if (is.null(dim(x))) {
        nm = names2(x)
    } else {
        nm = rownames2(x)
    }
    out = cbind(data.frame(nm), data.frame(x))
    out = setColnames(out, c(name, value))
    if (as.data.table) {
        setDT(out)
        return(out)
    } else {
        return(out)
    }
}


#' @name ppng
#' @title kevin's modification of ppng
#'
#' height and width are specified in inches by default
#' resolution is specified as 300 by default
#'
#' @export
ppng = function (expr, filename = "plot.png", height = 10, width = 10,
                 dim = NULL, cex = 1, title = NULL,
                 h = height, w = width,
                 cex.title = 1, oma.scale = 0, units = "in", res = 300, oma.val = c(1,1,1,1), pars = list(), ...) {
    suppressWarnings({
        this.env = environment()
        if (length(cex) == 1) {
            cex = rep(cex, 2)
        }
        height = h
        width = w
        height = cex[1] * height
        width = cex[2] * width
        DEFAULT.OUTDIR = Sys.getenv("PPNG.DIR")
        if (nchar(DEFAULT.OUTDIR) == 0)
            DEFAULT.OUTDIR = normalizePath("~/public_html/")
        if (!grepl("^[~/]", filename))
            filename = paste(DEFAULT.OUTDIR, filename, sep = "/")
        if (!file.exists(file.dir(filename)))
            system(paste("mkdir -p", file.dir(filename)))

        cat("rendering to", filename, "\n")
        old_oma = par(no.readonly=T)$oma
        lst.par = par(no.readonly=T)
        goodnm = intersect(names(pars), names(lst.par))
        lst.old.par = lst.par = lst.par[goodnm]
        oldpars = allpars = paste(unlist(lapply(seq_along(lst.par), function(i) {
            paste0(names(lst.par)[i], "=c(",
                   paste0(ifelse(is.na(lst.par[[i]]) | !is.character(lst.par[[i]]),
                                 lst.par[[i]], paste0("'", lst.par[[i]], "'")), collapse = ","), ")",
                   collapse = ",")
        })), collapse = ",")
        oldstr = paste0("par(", allpars, ")")
        ## on.exit({
        ##     for (i in seq_along(lst.old.par)) {
        ##         arg = paste0(names(lst.old.par)[i], "=c(",
        ##                      paste0(ifelse(is.na(lst.old.par[[i]]) | !is.character(lst.old.par[[i]]),
        ##                                    lst.old.par[[i]], paste0("'", lst.old.par[[i]], "'")), collapse = ","), ")",
        ##                      collapse = ",")
        ##         eval(parse(text = paste0("par(", arg, ")")), envir = this.env)
        ##     }

        ## })
        pf2 = parent.frame(2)
        on.exit({eval(parse(text = oldstr), envir = parent.frame(), enclos = pf2); reset.dev()})
        if (length(pars) > 0) {
            goodnm = intersect(names(pars), names(lst.par))
            lst.par[goodnm] = pars[goodnm]
            ## for (i in seq_along(lst.par)) {
            ##     arg = paste0(names(lst.par)[i], "=c(",
            ##                  paste0(ifelse(is.na(lst.par[[i]]) | !is.character(lst.par[[i]]),
            ##                                lst.par[[i]], paste0("'", lst.par[[i]], "'")), collapse = ","), ")",
            ##                  collapse = ",")
            ##     eval(parse(text = paste0("par(", arg, ")")), envir = parent.frame())
            ## }
            newpars = allpars = paste(unlist(lapply(seq_along(lst.par), function(i) {
                paste0(names(lst.par)[i], "=c(",
                       paste0(ifelse(is.na(lst.par[[i]]) | !is.character(lst.par[[i]]),
                                     lst.par[[i]], paste0("'", lst.par[[i]], "'")), collapse = ","), ")",
                       collapse = ",")
            })), collapse = ",")
            newstr = paste0("par(", allpars, ")")
            eval(parse(text = newstr), envir = parent.frame(), enclos = pf2)
        } else {
            newstr = ""
            newpars = ""
        }
        png(filename, height = height, width = width, units = units, res = res, ...) ## pointsize default is 12... maybe the default previously was 24?

        if (oma.scale > 0) {
            ## par(oma = oma.val * oma.scale)
            newpars = paste0(newpars, "oma=c(", paste0(oma.val * oma.scale, collapse = ","), ")", collapse = ",")
        }
        if (!is.null(dim)) {
            if (length(dim) == 1)
                dim = rep(dim, 2)
            dim = dim[1:2]
            layout(matrix(1:prod(dim), nrow = dim[1], ncol = dim[2],
                          byrow = TRUE))
        }
        ## eval(parse(text = paste0("{ par(", newpars, ");", as.character(as.expression(substitute(expr))), "}")),
        ##      envir = parent.frame())
        ## eval(parse(text = paste0("{ par(", newpars, ");", as.character(as.expression(substitute(expr))), "}")),
        ##      envir = parent.frame(), enclos = stackenv(parent.frame(2)))
        eval(parse(text = paste0("{ par(", newpars, ");", as.character(as.expression(substitute(expr))), "}")),
             envir = parent.frame(), enclos = pf2)
        ## eval(expr, envir = this.env)
        if (!is.null(title))
            title(title, cex.main = cex.title * max(cex))
        silent({dev.off()})
    })
}


file.dir <- function (paths) {
    return(gsub("(^|(.*\\/))?([^\\/]*)$", "\\2", paths))
}

#' @name ppdf
#' @title kevin's modification of ppdf
#'
#' height and width are specified in inches by default
#' resolution is specified as 300 but is not used
#'
#' @export
ppdf = function (expr, filename = "plot.pdf", height = 10, width = 10,
                 h = height, w = width,
                 cex = 1, title = NULL, byrow = TRUE, dim = NULL, cex.title = 1,
                 oma.scale = 0, oma.val = c(1,1,1,1), useDingbats = FALSE, res = 0, pars = list(), ...) {
    suppressWarnings({
        this.env = environment()
        if (length(cex) == 1)
            cex = rep(cex, 2)
        height = h
        width = w
        height = cex[1] * height
        width = cex[2] * width
        DEFAULT.OUTDIR = Sys.getenv("PPDF.DIR")
        if (nchar(DEFAULT.OUTDIR) == 0)
            DEFAULT.OUTDIR = normalizePath("~/public_html/")
        if (!grepl("^[~/]", filename))
            filename = paste(DEFAULT.OUTDIR, filename, sep = "/")
        if (!file.exists(file.dir(filename)))
            system(paste("mkdir -p", file.dir(filename)))
        cat("rendering to", filename, "\n")

        old_oma = par(no.readonly=T)$oma
        lst.par = par(no.readonly=T)
        goodnm = intersect(names(pars), names(lst.par))
        lst.old.par = lst.par = lst.par[goodnm]
        oldpars = allpars = paste(unlist(lapply(seq_along(lst.par), function(i) {
            paste0(names(lst.par)[i], "=c(",
                   paste0(ifelse(is.na(lst.par[[i]]) | !is.character(lst.par[[i]]),
                                 lst.par[[i]], paste0("'", lst.par[[i]], "'")), collapse = ","), ")",
                   collapse = ",")
        })), collapse = ",")
        oldstr = paste0("par(", allpars, ")")
        ## on.exit({
        ##     for (i in seq_along(lst.old.par)) {
        ##         arg = paste0(names(lst.old.par)[i], "=c(",
        ##                      paste0(ifelse(is.na(lst.old.par[[i]]) | !is.character(lst.old.par[[i]]),
        ##                                    lst.old.par[[i]], paste0("'", lst.old.par[[i]], "'")), collapse = ","), ")",
        ##                      collapse = ",")
        ##         eval(parse(text = paste0("par(", arg, ")")), envir = this.env)
        ##     }

        ## })
        pf2 = parent.frame(2)
        on.exit({eval(parse(text = oldstr), envir = parent.frame(), enclos = pf2); reset.dev()})
        if (length(pars) > 0) {
            goodnm = intersect(names(pars), names(lst.par))
            lst.par[goodnm] = pars[goodnm]
            ## for (i in seq_along(lst.par)) {
            ##     arg = paste0(names(lst.par)[i], "=c(",
            ##                  paste0(ifelse(is.na(lst.par[[i]]) | !is.character(lst.par[[i]]),
            ##                                lst.par[[i]], paste0("'", lst.par[[i]], "'")), collapse = ","), ")",
            ##                  collapse = ",")
            ##     eval(parse(text = paste0("par(", arg, ")")), envir = parent.frame())
            ## }
            newpars = allpars = paste(unlist(lapply(seq_along(lst.par), function(i) {
                paste0(names(lst.par)[i], "=c(",
                       paste0(ifelse(is.na(lst.par[[i]]) | !is.character(lst.par[[i]]),
                                     lst.par[[i]], paste0("'", lst.par[[i]], "'")), collapse = ","), ")",
                       collapse = ",")
            })), collapse = ",")
            newstr = paste0("par(", allpars, ")")
            eval(parse(text = newstr), envir = parent.frame(), enclos = pf2)
        } else {
            newstr = ""
            newpars = ""
        }
        pdf(filename, height = height, width = width,
            useDingbats = useDingbats, ...) ## pointsize default is 12

        if (oma.scale > 0) {
            newpars = paste0(newpars, "oma=c(", paste0(oma.val * oma.scale, collapse = ","), ")", collapse = ",")
        }
        if (!is.null(dim)) {
            if (length(dim) == 1)
                dim = rep(dim, 2)
            dim = dim[1:2]
            graphics::layout(matrix(1:prod(dim), nrow = dim[1], ncol = dim[2],
                                    byrow = byrow))
        }
        ## eval(expr)
        ## eval(parse(text = paste0("{", newstr, ";", as.character(as.expression(substitute(expr))), "}")),
        ## envir = parent.frame())
        ## eval(parse(text = paste0("{ par(", newpars, ");", as.character(as.expression(substitute(expr))), "}")),
        ##      envir = parent.frame(), enclos = stackenv(parent.frame(2)))
        eval(parse(text = paste0("{ par(", newpars, ");", as.character(as.expression(substitute(expr))), "}")),
             envir = parent.frame(), enclos = pf2)
        if (!is.null(title))
            title(title, cex.main = cex.title * max(cex))
        silent({dev.off()})
    })
}

#' @name names2
#' @title robust name()
#'
#' gives back character vector same length of input regardless whether named or not
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
#' @export
names2 = function(x) {
    nm = names(x)
    if (is.null(nm))
        return(rep("", length.out = length(x)))
    else
        return(nm)
}

#' @name rownames2
#' @title robust rownames
#'
#' gives back character vector same number of rows of input regardless whether named or not
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
#' @export
rownames2 = function(x) {
    if (!is.null(dim(x)))
        nm = rownames(x)
    else
        nm = names(x)
    if (is.null(nm))
        return(rep("", length.out = len(x)))
    else
        return(rep(nm, length.out = len(x)))
}

#' @name colnames2
#' @title robust colnames
#'
#' gives back character vector same number of columns of input regardless whether named or not
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
#' @export
colnames2 = function(x) {
    nm = colnames(x)
    if (is.null(nm))
        return(rep("", length.out = NCOL(x)))
    else
        return(nm)
}

#' @name names2<-
#' @title robust name() assignment
#'
#' similar to rlang::names2
#'
#' @param x vector
#' @return x a vector with all names
#' @export
`names2<-` = function(x, value, useempty = FALSE) {
    names(x) = if (!is.null(value))
                   rep(value, length.out = length(x))
               else {
                   if (useempty)
                       rep("", length.out = length(x))
               }
    return(x)
}

#' @name rownames2<-
#' @title robust rownames() assignment
#'
#'
#' @param x vector or matrix
#' @return x a vector fully named or rownamed
#' @export
`rownames2<-` = function(x, value, useempty = FALSE) {
    if (!is.null(dim(x))) {
        rownames(x) = if (!is.null(value))
                       rep(value, length.out = nrow(x))
                   else {
                       if (useempty)
                           rep("", length.out = nrow(x))
                   }
    } else {
        names(x) = if (!is.null(value))
                       rep(value, length.out = length(x))
                   else {
                       if (useempty)
                           rep("", length.out = length(x))
                   }
    }
    return(x)
}

#' @name colnames2<-
#' @title robust colnames() assignment
#'
#'
#' @param x data with dimensions
#' @return x data fully colnamed
#' @export
`colnames2<-` = function(x, value, useempty = FALSE) {
    colnames(x) = if (!is.null(value))
                      rep(value, length.out = ncol(x))
                  else {
                      if (useempty)
                          rep("", length.out = ncol(x))
                  }
    return(x)
}

#' @name rm_mparen
#' @title utility function for removing multiple parantheses
#'
#' probably not necessary
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
#' @export
rm_mparen  = function(str) {
    return(gsub('\\/{2,}', "/", str))
}

#' @name make_xfold
#' @title make training/test splits
#'
#' @author Kevin Hadi
#' @param dat data.table or data.frame of one row per observation
#' @param k number of groups
#' @return a list of row ids corresponding to each fold and training and test split
#' @export
make_xfold = function(dat, k = 10, nested = FALSE, times = 1, transpose = TRUE, seed = 10) {
  obs.id = seq_along2(dat)
  set.seed(seed)
  if (!nested) {
    train = caret::createFolds(obs.id, k = k, returnTrain = T)
  } else
    train = caret::createMultiFolds(obs.id, k = k, times = times)
  test = lapply(train, function(x) {
    setdiff(obs.id, x)
  })
  out = list(train = train, test = test)
  if (transpose)
    return(purrr::transpose(out))
  else
    return(out)
}


#' @name set_rngseed
#' @title set random number generator AND seed
#'
#' @author Kevin Hadi
#' @param rngkind string specifying number generator
#' @param seed integer specifying seed
#' @param use.old.sample.kind logical specifying discrete uniform generation method from prior to R361
#' @return a list of row ids corresponding to each fold and training and test split
#' @export
set_rngseed = function(seed = 10, rngkind = "L'Ecuyer-CMRG", normal.kind = "Inversion",
                       sample.kind = "Rejection", use.old.sample.kind = FALSE,
                       verbose = TRUE)
{
    if ("sample.kind" %in% names(formals(RNGkind))) {
        if (use.old.sample.kind) {
            sample.kind = "Rounding"
            if (verbose) message("Using default sample.kind from <R-3.6.0 (Rounding)")
        }
        RNGkind(rngkind, normal.kind, sample.kind)
        vb = RNGkind()
        if (verbose)  {
            message("RNG: ", vb[1])
            message("normal.kind: ", vb[2])
            message("sample.kind: ", vb[3])
        }
    } else {
        RNGkind(rngkind, normal.kind)
        vb = RNGkind()
        if (verbose)  {
            message("RNG: ", vb[1])
            message("normal.kind: ", vb[2])
        }
    }
    set.seed(seed)
    if (verbose)
        message("seed: ", seed)
    return(invisible(NULL))
}

#' @name make_ttsplit
#' @title make training/test splits
#' 
#' @author Kevin Hadi
#' @param dat data.table or data.frame of one row per observation. assumes each observation is a row
#' @param k number of groups
#' @return a list of row ids corresponding to each fold and training and test split
#' @export
make_ttsplit = function(dat,
                        field = NULL, use.index = TRUE, k = 10,
                        nested = FALSE, times = 1, transpose = TRUE,
                        seed = 10, partition_prob = 0.632,
                        split_type = c("crossfold", "resample", "partition"),
                        as.data.table = FALSE, rngkind = "L'Ecuyer-CMRG") {
    
    current.rng = .Random.seed
    txt = parse(text = sprintf(".Random.seed = as.integer(%s)", mkst(current.rng)))
    set_rngseed(seed = seed, rngkind = rngkind)
    on.exit(eval(txt, envir = parent.frame()))
    
    o_split_type = c("crossfold", "resample", "partition")
    obs.id = seq_along2(dat)
    
    set.seed(seed)

    wtf = setdiff(split_type, o_split_type)
    if (length(wtf)) {
        warning("split_type should be one of ", paste(o_split_type, collapse = " "))
        split_type = split_type[split_type %in% o_split_type]
    }

    if (!nested & times != 1) times = 1

    if (NROW(split_type) > 1) {
        split_type = split_type[1]
        message("more than one split type designated, using ", split_type)
    }

    if (split_type == "crossfold") {
        if (!nested) {
            train = caret::createFolds(obs.id, k = k, returnTrain = T)
        } else
            train = caret::createMultiFolds(obs.id, k = k, times = times)
    } else if (split_type == "resample") {
        train = caret::createResample(obs.id, times * k)
    } else if (split_type == "partition") {
        train = caret::createDataPartition(obs.id, times * k, p = partition_prob)
    }
    
    test = lapply(train, function(x) {
        setdiff(obs.id, x)
    })

    trainl = list()
    testl = list()
    if (use.index == FALSE) {
        if (!is.null(ncol(dat))) {
            if (!is.null(field)) {
                trainl = lapply(train, function(x) dat[x,])
                testl = lapply(test, function(x) dat[x,])
            } else {
                if (anyDuplicated(dat[[field]])) warning("some indices are duplicated")
                trainl = lapply(train, function(x) setNames(dat[x,][[field]], x))
                testl = lapply(test, function(x) setNames(dat[x,][[field]], x))
            }
        } else {
            trainl = lapply(train, function(x) setNames(dat[x], x))
            testl = lapply(test, function(x) setNames(dat[x], x))
        }
    }

    if (as.data.table) {
        dt.train = stack.dt(train)[, set := "train"]
        dt.train$id = rep_len2(stack.dt(trainl)$values, dt.train)
        dt.test = stack.dt(test)[, set := "test"]
        dt.test$id = rep_len2(stack.dt(testl)$values, dt.test)
        return(
            setcols(
                rbind(dt.train, dt.test),
                c("ix", "partition", "set", "id"))
        )
    }

    if (NROW(trainl) && NROW(testl))
        out = list(train = trainl, test = testl)
    else
        out = list(train = train, test = test)
    if (transpose)
        return(purrr::transpose(out))
    else
        return(out)
}

#' @name aggregate_roc
#' @title make aggregated roc curve
#'
#' @author Kevin Hadi
#' @param dat data.table or data.frame of one row per observation. assumes each observation is a row
#' @param k number of groups
#' @return a list of row ids corresponding to each fold and training and test split
#' @export
aggregate_roc = function(dat, subgroup.field = "Method", score = "BRCA1", lab = "fmut_brca1", mc.cores = 1, only_unique = F, include_group = FALSE, return_raw = FALSE) {
    nms = unique(dat[[subgroup.field]])
    out = rbindlist(mclapply(nms, function(nm) {
        x = copy(dat[Method == nm])
        for (i in seq_len(NROW(score))) {
            cut = santoku::chop_evenly(x[[score[i]]], intervals = 50)
            rescore = normv(f2int(cut))
            x[[score[i]]] = rescore
        }
        if (isTRUE(return_raw))
            return(x)
        outt = make_roc(x, lab = lab, score = score, include_group = include_group)[, Method := nm]
        if (only_unique) {
            outt = rbind(outt[!duped(prd)], outt[1][, c("Specificity", "Sensitivity") := list(Specificity = 0, Sensitivity = 1)])
        }
        return(outt)
    }, mc.cores = mc.cores))
}


#' @name %=%
#' @title test if two vectors are equal (uses conversion to character)
#'
#' Robust to NA
#'
#' @author Kevin Hadi
#' @param x a vector
#' @param y a vector
#' @return logical vector
#' @export
`%=%` = function(x,y) {
    paste(x) == paste(y)
}


#' @name seq_along2
#' @title seq along either row of table or length of vector
#'
#'
#' @author Kevin Hadi
#' @param x data
#' @return vector
#' @export
seq_along2 = function(x)  {
  seq_len(NROW(x))
}


#' @name rep_each
#' @title recycle vector - shortcut for rep(x, each = each)
#'
#' @description
#'
#' @author Kevin Hadi
#' @param x data
#' @param each length to extend vector by
#' @return vector
#' @export
rep_each = function(x, each) {
    return(rep(x, each = each))
}


#' @name rep_len
#' @title recycle vector - overload base::rep_len
#'
#' @description
#' problem with base::rep_len is that it doesn't work with other objects
#'
#' @author Kevin Hadi
#' @param x data
#' @param length.out length to extend vector by
#' @return vector
#' @export
rep_len = function(x, length.out) {
    return(rep(x, length.out = length.out))
}

#' @name rep_len2
#' @title recycle vector along length OR nrow of object
#'
#'
#' @author Kevin Hadi
#' @param x data
#' @param objalong any object to recycle x along if uselen = TRUE, or an actual integer value if uselen = FALSE
#' @return vector
#' @export
rep_len2 = function(x, objalong, uselen = TRUE) {
    if (uselen)
        rep(x, length.out = NROW(objalong))
    else
        rep(x, length.out = objalong)
}

#' @name file.exists2
#' @title slightly more robust test for whether file exists
#'
#' test whether file exists. "/dev/null", NA, "NA", "NULL", values excluded
#' by default.
#'
#' @author Kevin Hadi
#' @param x a character vector
#' @return logical
#' @export file.exists2
file.exists2 = function(x, nullfile = "/dev/null", bad = c(NA, "NA", "NULL", "")) {
    return(!file.not.exists(x = x, nullfile = nullfile, bad = bad))
}


#' @name file.not.exists
#' @title slightly more robust test for whether file does not exist
#'
#' test whether a file is NA, NULL, or /dev/null OR if
#' the file exists
#'
#' @author Kevin Hadi
#' @param x a character vector
#' @return logical
#' @export file.not.exists
file.not.exists = function(x, nullfile = "/dev/null", bad = c(NA, "NA", "NULL", "")) {
    isnul = (is.null(x))
    ## isbadfile =
    ##     (x %in% bad | x == nullfile) |
    ##     (x != nullfile & !file.exists(as.character(x)))
    isbadfile = (x %in% bad | x == nullfile)
    isgoodfile = which(!isbadfile)
    isbadfile[isgoodfile] = !file.exists(as.character(x[isgoodfile]))
    isnolength = len(x) == 0
    return(isnul | isnolength | isbadfile)
}




#' @name silent
#' @title run expression without any printed output
#'
#' execute expression without any output to console.
#' silent({var = function_that_has_explicit_print(...)})
#'
#'
#' @author Kevin Hadi
#' @param ... an expression
#' @return NULL
#' @export
silent <- function (this_expr, this_env = parent.frame(), enclos = parent.frame(2)) {
        eval(expr = {
            suppressWarnings(capture.output(capture.output(... = this_expr, file = "/dev/null", 
                                          type = c("output")), file = "/dev/null", type = "message"))
        }, envir = .GlobalEnv, enclos = .GlobalEnv)
        invisible()
}


#' @name overwriteR6
#' @title overwrite a method in R6 class generator
#'
#' useful for dev purposes.
#'
#' @export overwriteR6
overwriteR6 = function(newfun, oldfun, r6gen, meth = "public_methods", package = NULL, envir = globalenv()) {
    meth = ifelse(grepl("^pub", meth), "public_methods",
           ifelse(grepl("^pri", meth), "private_methods",
           ifelse(grepl("^act", meth), "active",
                  NA_character_)))
    if (is.na(meth))
        stop("method must refer to public, private, or active method")
    if (!is.null(package)) {
        if (is.character(package))
            envpkg = asNamespace(package)
        else if (isNamespace(package))
            envpkg = package
        nmpkg = environmentName(envpkg)
    }
    r6 = get(r6gen)
    tmpfun = r6[[meth]][[oldfun]]
    .newfun = get(newfun)
    environment(.newfun) = environment(tmpfun)
    attributes(.newfun) = attributes(tmpfun)
    r6[[meth]][[oldfun]] = .newfun
    NULL
}

#' @name copy2
#' @title make deep copy
#'
#' useful for dev
#' makes deep copy of R6 object, S4 object, or anything else really
#'
#' @export copy2
copy2 = function(x) {
    if (inherits(x, "R6")) {
        x2 = x$clone(deep = T)
        for (name in intersect(names(x2$.__enclos_env__), c("private", "public")))
            for (nname in names(x2$.__enclos_env__[[name]]))
                tryCatch({
                    x2$.__enclos_env__[[name]][[nname]] = rlang::duplicate(x2$.__enclos_env__[[name]][[nname]])
                }, error = function(e) NULL)
        return(x2)
    } else if (isS4(x)) {
        x2 = rlang::duplicate(x)
        slns = slotNames(x2)
        for (sln in slns) {
            tryCatch({slot(x2, sln) = rlang::duplicate(slot(x2, sln))},
                     error = function(e) NULL)
        }
        return(x2)
    } else {
        x2 = rlang::duplicate(x)
        return(x2)
    }
}

#' @name copy3
#' @title make deep copy, recursively
#'
#' useful for dev
#' makes deep copy of R6 object, S4 object, or anything else really
#'
#' @export copy3
copy3 = function (x, recurse_list = TRUE) {
    if (inherits(x, "R6")) {
        x2 = rlang::duplicate(x$clone(deep = T))
        for (name in intersect(names(x2$.__enclos_env__), c("private", 
            "public"))) for (nname in names(x2$.__enclos_env__[[name]])) tryCatch({
            x2$.__enclos_env__[[name]][[nname]] = copy3(x2$.__enclos_env__[[name]][[nname]])
        }, error = function(e) NULL)
        return(x2)
    } else if (isS4(x)) {
        x2 = rlang::duplicate(x)
        slns = slotNames(x2)
        for (sln in slns) {
            tryCatch({
                slot(x2, sln) = copy3(slot(x2, sln))
            }, error = function(e) NULL)
        }
        return(x2)
    } else if (inherits(x, c("list"))) {
        x2 = rlang::duplicate(x)
        x2 = rapply(x2, copy3, how = "replace")
        return(x2)
    } else {
        x2 = rlang::duplicate(x)
        return(x2)
    }
}

#' @name peepr6
#' @title peepr6
#'
#' useful for dev
#'
#' @export peepr6
peepr6 = function(x) {
    if (inherits(x, "R6")) {
        return(x$.__enclos_env__)
    } else {
        message("object is not R6...")
        return(x)
    }
}

#' @name copyr6
#' @title make deep copy of all non-function public and private fields in R6
#'
#' useful for dev
#'
#' @export copyr6
copyr6 = function(x) {
    if (inherits(x, "R6")) {
        x2 = x$clone(deep = T)
        for (name in intersect(names(x2$.__enclos_env__), c("private", "public")))
            for (nname in names(x2$.__enclos_env__[[name]]))
                tryCatch({
                    x2$.__enclos_env__[[name]][[nname]] = rlang::duplicate(x2$.__enclos_env__[[name]][[nname]])
                }, error = function(e) NULL)
        return(x2)
    } else {
        message("object is not R6...")
        return(x)
    }
}

#' @name copys4
#' @title make deep copy of all private slots in s4 object
#'
#' useful for dev
#'
#' @export copys4
copys4 = function(x) {
    if (isS4(x)) {
        x2 = rlang::duplicate(x)
        slns = slotNames(x2)
        for (sln in slns) {
            tryCatch({slot(x2, sln) = rlang::duplicate(slot(x2, sln))},
                     error = function(e) NULL)
        }
        return(x2)
    } else {
        message("object is not s4...")
        return(x)
    }
}


#' @name overwritefun
#' @title overwrite a function in its namespace
#'
#' useful for dev purposes.
#'
#' @export overwritefun
overwritefun = function (newfun, oldfun, package, envir = globalenv())
{
    if (is.character(newfun) && is.character(oldfun) && missing(package))
        stop("must specify package for oldfun")
    if (!missing(package)) {
        if (is.character(package))
            envpkg = asNamespace(package)
        else if (isNamespace(package))
            envpkg = package
    } else {
        if (missing(package)) {
            envpkg = asNamespace(environment(oldfun))
        }
    }
    if (!is.character(oldfun)) {
        oldfun = deparse(tail(as.list(substitute(oldfun)), 1)[[1]])
    }
    if (!is.character(newfun)) {
        newfunenv = asNamespace(environment(newfun))
        newfun = deparse(tail(as.list(substitute(newfun)), 1)[[1]])
    } else {
        newfunenv = parent.frame()
    }
    nmpkg = environmentName(envpkg)
    tmpfun = get(oldfun, envir = envpkg)
    .newfun = get(newfun, envir = newfunenv)
    environment(.newfun) = environment(tmpfun)
    attributes(.newfun) = attributes(tmpfun)
    evalq(asn2(oldfun, .newfun, ns = nmpkg), environment(), parent.frame())
    globasn(.newfun, oldfun, vareval = T)
}

## overwritefun = function(newfun, oldfun, package, envir = globalenv()) {
##     if (is.character(package))
##         envpkg = asNamespace(package)
##     else if (isNamespace(package))
##         envpkg = package
##     nmpkg = environmentName(envpkg)
##     tmpfun = get(oldfun, envir = envpkg)
##     .newfun = get(newfun)
##     environment(.newfun) = environment(tmpfun)
##     attributes(.newfun) = attributes(tmpfun)
##     eval(asn2(oldfun, .newfun, ns = nmpkg), globalenv())
##     globasn(.newfun, oldfun, vareval = T)
## }




#' @name write.ctab
#' @title writing a comma separated table with quotes
#'
#' comma-separated table with quotes around strings
#'
#' @export write.ctab
write.ctab = function (x, ..., sep = ",", quote = T, row.names = F)
{
    if (!is.data.frame(x))
        x = as.data.frame(x)
    write.table(x, ..., sep = sep, quote = quote, row.names = row.names)
}




#' @name qmat
#' @title query a matrix with nonmatching entries as NA
#'
#'
#' @export
qmat = function(mat, rid = NULL, cid = NULL) {
    rown_provided = FALSE
    coln_provided = FALSE
    ## if (is.null(rid)) {
    ##     rid = seq_len(nrow(mat))
    ## } else if (is.character(rid)) {
    ##     rid = setNames(match3(rid, rownames(mat)), rid)
    ##     rown_provided = TRUE
    ## }
    if (is.character(rid)) {
        rid = setNames(match3(rid, rownames(mat)), rid)
        rown_provided = TRUE
    }
    ## rst = mkst(rid)
    ## if (is.null(cid))
    ##     cid = seq_len(ncol(mat))
    ## else if (is.character(cid)) {
    ##     coln_provided = TRUE
    ##     cid = setNames(match3(cid, colnames(mat)), cid)
    ## }
    if (is.character(cid)) {
        coln_provided = TRUE
        cid = setNames(match3(cid, colnames(mat)), cid)
    }
    ## if (!inherits(rid, "integer")) rid = as.integer(rid)
    ## if (!inherits(cid, "integer")) cid = as.integer(cid)
    ## can work with data table
    if (!inherits(rid, "character")) {
        rid = structure(as.character(rid), names = names(rid))
        rid[is.na(rid)] = "NA_integer_"
    }
    if (!inherits(cid, "character")) {
        cid = structure(as.character(cid), names = names(cid))
        cid[is.na(cid)] = "NA_integer_"
    }
    out = et(sprintf("mat[%s,%s,drop = FALSE]", mkst(rid), mkst(cid)))
    ## out = mat[rid,cid,drop = FALSE]
    if (rown_provided) rownames(out) = names(rid)
    if (coln_provided) colnames(out) = names(cid)
    return(out)
}


#' @name match3
#' @title similar to setkey except a general use utility
#'
#' very slow version of keying a la data.table
#' but for general/interactive use
#'
#' @export
match3 = function(x, table, nomatch = NA_integer_, old = TRUE, use.data.table = TRUE) {
  out = if (use.data.table) {
    tryCatch({
      dx = data.table(x = x)[, id.x := seq_len(.N)]
      dtb = data.table(table = table)[, id.tb := seq_len(.N)]
      ## setkey(dx, x)[list(dtb$table)]$id.x
      setkey(dtb, table)[list(dx$x)]$id.tb
    }, error = function(e) structure("err", class = "err"))
  }
  if (!is.null(out) && !inherits(out, "err")) return(out)
  if (old) {
    dx = within(data.frame(x = x), {id.x = seq_along(x)})
    dtb = within(data.frame(table = table), {id.tb = seq_along(table)})
    res = merge(dx, dtb, by.x = "x", by.y = "table", all.x = TRUE,
      allow.cartesian = TRUE)
    return(res$id.tb[order(res$id.x)])
  } else  {
    m = match(table,x)
    mat = cbind(m, seq_along(m))
    mat = mat[!is.na(mat[,1]),,drop=FALSE]
    mat = mat[order(mat[,1], na.last = FALSE),,drop = FALSE]
    mat = cbind(mat, seq_len(dim(mat)[1]))
    m2 = match(x,table)
    ix = which(!duplicated(m2) & !is.na(m2))
    mat_rix = unlist(rep(split(mat[,3], mat[,1]), base::tabulate(m2)[m2][ix]))
    ## mat_rix = unlist(rep(split(mat[,3], mat[,1]), base::tabulate(m2)[m2][ix]))
    ix = rep(1, length.out = length(m2))
    ## original line
    ## ix[!is.na(m2)] = base::tabulate(m)[!is.na(m2)]
    ix[!is.na(m2)] = base::tabulate(m)[m][m2][!is.na(m2)]
    out = rep(m2, ix)
    out[!is.na(out)] = mat[mat_rix,,drop=F][,2]
    return(out)
    ## m = match(table, x)
    ## mat = cbind(m, seq_along(m))
    ## mat = mat[!is.na(mat[, 1]), , drop = FALSE]
    ## mat = mat[order(mat[, 1]), , drop = FALSE]
    ## mat = cbind(mat, seq_len(dim(mat)[1]))
    ## m2 = match(x, table)
    ## ix = which(!duplicated(m2))
    ## mat_rix = unlist(rep(split(mat[, 3], mat[, 1]), base::tabulate(m2)[m2][ix]))
    ## mat[mat_rix, , drop = F][, 2]
  }
}

#' @name %K%
#' @title similar to setkey except a general use utility
#'
#' slower version of setkey, but for interactive use
#'
#' @export
`%K%` = function(thisx,thisy, old = TRUE) {
    ## m = match(x,y)
    ## mat = cbind(m, seq_along(m))
    ## mat = mat[!is.na(mat[,1]),,drop=FALSE]
    ## mat = mat[order(mat[,1]),,drop = FALSE]
    ## mat = cbind(mat, seq_len(dim(mat)[1]))
    ## ## rleseq(x[which(x %in% y)], clump = T)
    ## m2 = match(y,x)
    ## ## lst = rleseq(m2, clump = T)
    ## ix = which(!duplicated(m2))
    ## base::tabulate(m2)[m2][ix]
    ## mat_rix = unlist(rep(split(mat[,3], mat[,1]), base::tabulate(m2)[m2][ix]))
    ## mat[mat_rix,,drop=F][,2]
    if (old)
        return(match3(table = thisx, x = thisy, old = TRUE))
    else
        return(match3(table = thisx, x = thisy, old = FALSE))
}

#' @name column_to_rownames
#' @title making column into rownames
#'
#' internal version that doesn't require library(tibble)
#'
#' @param .data a data frame/table
#' @return a data frame/table with rownames from a column
#' @export
column_to_rownames = function(.data, var = "rowname", force = T, sep = " ") {
  ## if (inherits(.data, c("data.frame", "DFrame"))) {
  if (!is.null(dim(.data))) {
    tmpfun = function(...) paste(..., sep = sep)
    if (!is.null(rownames(.data)) || force) {
      ## rn = .data[[var]]
      if (is.numeric(var)) {
        eva = eval(parse(text = paste(".data[,", paste("c(", paste0(var, collapse = ", "), ")"), ",drop=FALSE]")))
        if (ncol(eva) > 1) eva = dodo.call2(dg(tmpfun), eva)
        rn = unname(unlist(eva))
        colix = setdiff(seq_len(ncol(.data)), var)
      } else if (is.character(var)) {
        eva = eval(parse(text = paste(".data[,", paste("c(", paste0(paste0("\"", var, "\""), collapse = ", "), ")"), ",drop=FALSE]")))
        if (ncol(eva) > 1) eva = dodo.call2(dg(tmpfun), eva)
        rn = unname(unlist(eva))
        colix = setdiff(seq_len(ncol(.data)), match3(var,colnames(.data)))
      }
      eval(parse(text = paste(".data = .data[,", paste("c(", paste0(colix, collapse = ", "), ")"), ", drop = FALSE]")))
      ## .data = .data[, colix,drop = FALSE]
      if (inherits(.data, "tbl"))
        .data = as.data.frame(.data)
      if (inherits(.data, "data.frame")) {
        rownames(.data) = make.unique(replace(as.character(rn), is.na(rn), "NA"))
      } else {
        rownames(.data) = replace(as.character(rn), is.na(rn), "NA")
      }
      return(.data)
    } else
      return(.data)
  } else
    stop("must be a data frame-like object")
}

#' @name col2rn
#' @title alias for column_to_rownames
#'
#' internal version that doesn't require library(tibble)
#'
#' @param .data a data frame/table
#' @return a data frame/table with rownames from a column
#' @export
col2rn = column_to_rownames


#' @name rownames_to_column
#' @title making column out of rownames
#'
#' internal version that doesn't require library(tibble)
#'
#' @param .data a data frame/table
#' @return a data frame/table with the rownames as an additional column
#' @export
rownames_to_column = function(.data, var = "rowname", keep.rownames = FALSE,
                              asdf = as.data.frame, as.data.frame = FALSE) {
    ## if (inherits(.data, c("data.frame", "DFrame"))) {
    as.data.frame = asdf
    if (!is.null(dim(.data))) {
        if (!is.null(rownames(.data))) {
            rn = rownames(.data)
            if (as.data.frame)
                .data = cbind(u.var5912349879872349876 = rn, as.data.frame(.data, row.names = make.unique(rn)))
            else
                .data = cbind(u.var5912349879872349876 = rn, .data)
            colnames(.data)[1] = var
            if (keep.rownames)
                rownames(.data) = rn
            return(.data)
        } else
            return(.data)
    } else
        stop("must be a data frame-like object")
}

#' @name rn2col
#' @title alias for rownames_to_column
#'
#' internal version that doesn't require library(tibble)
#'
#' @param .data a data frame/table
#' @return a data frame/table with rownames from a column
#' @export
rn2col = rownames_to_column



#' @name normpath
#' @title normalize directory, but not basepath
#'
#' get the absolute file directory without following the
#' base path link
#'
#' @param str a path string
#' @return a normalized path
#' @export
normpath = function(p) {
    fe = file.exists2(p)
    bn = basename(p)
    d = dirname(normalizePath(p))
    return(ifelse(fe, paste0(d, "/", bn), as.character(p)))
    ## return(paste0(d, "/", bn))
}




#' @name rm_mparen
#' @title remove multiple parentheses from path
#'
#' utility function for removing multiple parantheses
#' probably not necessary
#'
#' @param str a path string
#' @return a string with multiple parentheses replaced with a single parenthesis
rm_mparen  = function(str) {
    return(gsub('\\/{2,}', "/", str))
}

#' @name numeq
#' @title test equality between numeric values with some tolerance
#'
#' @description
#' two numerical values may be slightly off in their decimal precision.
#' These may be considered equivalent values but the `==` operator will
#' return FALSE. This tests for equivalence of two values with some lower
#' tolerance limit
#'
#' @return logical vector
#' @export
numeq = function(x, y, tol = .Machine$double.eps^0.5) {
    abs(x - y) < tol
}


#' @name symdiff
#' @title data.table of all setdiff items in X and in Y
#'
#' gives back data table of setdiff elements
#' noting whether the element is in vector x or vector y
#' This gives back all elements, including non-unique
#'
#' @return data.table
#'
#' @export
symdiff = function(x, y, ignore.na = FALSE) {
    xy = setdiff(x,y)
    yx = setdiff(y,x)
    if (ignore.na) {
        xy = na.omit(xy)
        yx = na.omit(yx)
    }
    elx = x[which(x %in% xy)]
    ely = y[which(y %in% yx)]
    if (length(xy)) {
        xy = data.table(elements = elx,
                        ix.x = which(x %in% xy),
                        ix.y = NA_integer_,
                        inx = TRUE, iny = FALSE)
        lst = rleseq(xy$elements, clump = T)
        xy = cbind(xy, as.data.table(lst))
    } else
        xy = data.table()
    if (length(yx)) {
        yx = data.table(elements = yx,
                        ix.x = NA_integer_,
                        ix.y = which(y %in% yx),
                        inx = FALSE, iny = TRUE)
        lst = rleseq(yx$elements, clump = T)
        yx = cbind(yx, as.data.table(lst))
    } else
        yx = data.table()
    tb = rbind(xy,
               yx, fill = T)
    return(tb)
}


#' @name debug.s4
#' @title debug an S4 function
#'
#' wrapper around trace
#'
#' @export debug.s4
debug.s4 = function(what, signature, where) {
  trace(what = what, tracer = browser, at = 1, signature = signature, where = where)
}

#' @name undebug.s4
#' @title undebug an S4 function
#'
#' wrapper around untrace
#'
#' @export undebug.s4
undebug.s4 = function(what, signature, where) {
  untrace(what = what, signature = signature, where = where)
}


#' @name interaction2
#' @title interaction but orders levels based on input vectors
#'
#' @description
#' Same as base::interaction but orders levels based on the appearance of elements
#' in input vector(s)
#'
#' @export
interaction2 = function(..., drop = FALSE, sep = ".", lex.order = FALSE)
{
  args <- list(...)
  narg <- length(args)
  if (narg < 1L)
    stop("No factors specified")
  if (narg == 1L && is.list(args[[1L]])) {
    args <- args[[1L]]
    narg <- length(args)
  }
  for (i in narg:1L) {
    unix = which(!duplicated(args[[i]]))
    f <- factor(args[[i]], levels = args[[i]][unix])[, drop = drop]
    l <- levels(f)
    if1 <- as.integer(f) - 1L
    if (i == narg) {
      ans <- if1
      lvs <- l
    }
    else {
      if (lex.order) {
        ll <- length(lvs)
        ans <- ans + ll * if1
        lvs <- paste(rep(l, each = ll), rep(lvs, length(l)),
          sep = sep)
      }
      else {
        ans <- ans * length(l) + if1
        lvs <- paste(rep(l, length(lvs)), rep(lvs, each = length(l)),
          sep = sep)
      }
      if (anyDuplicated(lvs)) {
        ulvs <- unique(lvs)
        while ((i <- anyDuplicated(flv <- match(lvs,
          ulvs)))) {
            lvs <- lvs[-i]
            ans[ans + 1L == i] <- match(flv[i], flv[1:(i -
                                                         1)]) - 1L
            ans[ans + 1L > i] <- ans[ans + 1L > i] - 1L
          }
        lvs <- ulvs
      }
      if (drop) {
        olvs <- lvs
        lvs <- lvs[sort(unique(ans + 1L))]
        ans <- match(olvs[ans + 1L], lvs) - 1L
      }
    }
  }
  structure(as.integer(ans + 1L), levels = lvs, class = "factor")
}

#' @name lapply_dt
#' @title Flexibly apply function to columns of data.table/frame
#'
#' @description
#' Convenience function to apply a function to columns of a data.table/frame.
#' The syntax for the columns is flexible
#'
#' lapply_dt(c(newcolname = colname), dt, dosomething)
#' will give a data.table or list with the column name
#' renamed to newcolname
#' lapply_dt(.(newcolname = colname)...) also works as well.
#' Note that no quotations are needed.
#'
#' @param x fields to apply function to
#' @param dt data.table/frame
#' @param LFUN either a character name of a function, or a function
#' @param natype the type of NA which is to be specified as one of NA, NA_character_, NA_real_, or NA_integer_
#' @param evalcall logical flag to evaluate x as a call or not
#' @param as.data.table a logical indicating whether to coerce the output to a data.table
#' @return data.table/frame if as.data.table is TRUE, otherwise a list
#' @export lapply_dt
lapply_dt = function(x, dt, LFUN = "identity", natype = NA, as.data.table = T, evalcall = FALSE) {
    if (!is.function(LFUN))
        LFUN = base::mget(x = 'identity', mode = "function", inherits = T)[[1]]
    expr = substitute(x)
    if (is.name(expr))
        x = x
    else if (evalcall && is.call(expr))
        expr = eval(expr)
    else {
        x = trimws(gsub(',', "", unlist(strsplit(toString(expr), " "))[-1]))
        if (!is.null(names(expr)))
            names(x) = names(expr)[-1]
    }
    if (!is.null(names(x)))
        nm = names(x)
    else
        nm = x
    out = setNames(lst.emptyreplace(lapply(x, function(x, dt) {
        dt[[x]]
    }, dt = dt), natype), nm)
    out = lapply(out, LFUN)
    if (as.data.table)
        return(as.data.table(out))
    else
        return(out)
}


#' @name fix.cols
#' @title Fix messed up data frame/table column names
#'
#' @description
#' If there are any malformed columns
#' (e.g. those with numbers at the beginning
#' or a dash) these column names are fixed
#'
#'
#' @param dt data.table/frame
#' @param sep separator field
#' @return data.table/frame
#' @export fix.cols
fix.cols = function(dt, sep = "_") {
    this_sep = sep
    cl = colnames(dt)
    probs.num = grep("^[0-9]", cl)
    probs.dash = grep("-", cl)
    if (length(probs.num) | length(probs.dash)) {
        cl[probs.num] = paste0("X", this_sep, cl[probs.num])
        cl[probs.dash] = gsub("-", this_sep,  cl[probs.dash])
    }
    colnames(dt) = cl
    return(dt)
}


#' @name process_tbl
#' @title flexibly read in a field and append an id to the output
#'
#' @description
#'
#'
#' @param tbl table with fields to read in
#' @param field field to read in
#' @param id.field field with id to append to output
#' @param read.fun function to read in, will try to guess based on extension, but may need to provide
#' @param remove_ext extension strings to remove from file
#' @param mc.cores number of cores
#' @return a list of idx and seq
#'
#' @export
process_tbl = function(tbl, field = "jabba_rds", id.field = "pair", read.fun, remove_ext = c(".gz", ".zip"), mc.cores = 1) {
    forceall()
    ## invisible(eapply(environment(), force, all.names = TRUE))
    tbl = tbl[file.exists(get(field))]
    lst = with(tbl, {
        mclapply(mc.cores = mc.cores, subset2(dg(field,F), file.exists(x)), function(x, field = field, id.field = id.field, read.fun = read.fun, ...) {
            id.field = dg(id.field)
            field = dg(field)
            remove_ext = dg(remove_ext)
            if (missing(read.fun)) {
                remove_expr = paste(paste0(remove_ext, "$"), collapse = "|")
                fext = file_ext(gsub(remove_expr, "", x))
                read.fun = switch(fext, "vcf" = read_vcf, "rds" = readRDS, "txt" = fread,
                                  "csv" = fread, "tab" = fread)
                ##if expression isn't missing, eval expression
            }
            out = read.fun(x, ...)
            if (inherits(out, c("data.frame", "GRanges", "list")))
                out$pair = g2()[get(field) == x]$pair
            return(out)
        })})
    names(lst) = tbl[[id.field]]
    return(lst)
}

#' @name dedup
#' @title dedup
#'
#' @description
#' stolen from skitools
#'
#' @param x vector to dedup
#' @param suffix character separator
#' @return a vector
#' @author Marcin Imielinski
dedup = function(x, suffix = ".") {
    dup = duplicated(x)
    udup = setdiff(unique(x[dup]), NA)
    udup.ix = lapply(udup, function(y) which(x == y))
    udup.suffices = lapply(udup.ix, function(y) c("", paste(suffix,
        2:length(y), sep = "")))
    out = x
    out[unlist(udup.ix)] = paste(out[unlist(udup.ix)], unlist(udup.suffices),
        sep = "")
    return(out)
}



#' @name rand.string
#' @title make a random string
#'
#' @return random string
#' @author Someone from Stackoverflow
#' @export rand.string
rand.string = function(n=1, length=12)
{
    randomString <- c(1:n)                  # initialize vector
    for (i in 1:n)
    {
        randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
                                        length, replace=TRUE),
                                 collapse="")
    }
    return(randomString)
}


#' @name rleseq
#' @title numbers up within repeating elements of a vector
#'
#' @description
#' returns unique id within each unique element of a vector or set of provided vectors
#' and also a running id within each unique element
#'
#' @param ... Vector(s) to identify with unique id and a running id within each unique id
#' @param clump a logical specifying if duplicates are to be counted together
#' @param recurs a logical that is meant to only be set by the function when using clump = TRUE
#' @return a list of idx and seq
#' @author Kevin Hadi
#' @export
rleseq = function (..., clump = TRUE, recurs = FALSE, na.clump = TRUE, 
                   na.ignore = FALSE, sep = paste0(" ", rand.string(length = 6), 
                     " "), use.data.table = TRUE) 
{
  force(sep)
  out = if (use.data.table) {
    tryCatch(
    {
      dt = data.table(...)
      setnames(dt, make.names(rep("", ncol(dt)), unique = T))
      ## make.unique
      cmd = sprintf("dt[, I := .I][, .(idx = .GRP, seq = seq_len(.N), lns = .N, I), by = %s]", mkst(colnames(dt), "list"))
      dt = eval(parse(text = cmd))
      setkey(dt, I)[, .(idx, seq, lns)]
    }, error = function(e) structure("data table didn't work...", class = "err"))
  }
  if (!(is.null(out) || class(out)[1] == "err"))
    return(as.list(out))
  rand.string <- function(n = 1, length = 12) {
    randomString <- c(1:n)
    for (i in 1:n) {
      randomString[i] <- paste(sample(c(0:9, letters, LETTERS), 
        length, replace = TRUE), collapse = "")
    }
    return(randomString)
  }
  if (isTRUE(na.clump)) 
    paste = function(..., sep) base::paste(..., sep = sep)
  else paste = function(..., sep) base::paste(stringr::str_c(..., 
    sep = sep))
  lns = base::lengths(list(...))
  if (!all(lns == lns[1])) 
    warning("not all vectors provided have same length")
  fulllens = max(lns, na.rm = T)
  vec = setNames(paste(..., sep = sep), seq_len(fulllens))
  if (length(vec) == 0) {
    out = list(idx = integer(0), seq = integer(0), lns = integer(0))
    return(out)
  }
  if (na.ignore) {
    isnotna = which(rowSums(as.data.frame(lapply(list(...), 
      is.na))) == 0)
    out = list(idx = rep(NA, fulllens), seq = rep(NA, fulllens), 
      lns = rep(NA, fulllens))
    if (length(isnotna)) 
      vec = vec[isnotna]
    tmpout = do.call(rleseq, c(alist(... = vec), alist(clump = clump, 
      recurs = recurs, na.clump = na.clump, na.ignore = FALSE, use.data.table = FALSE)))
    for (i in seq_along(out)) out[[i]][isnotna] = tmpout[[i]]
    return(out)
  }
  if (!isTRUE(clump)) {
    rlev = rle(vec)
    if (isTRUE(recurs)) {
        ## return(unlist(unname(lapply(rlev$lengths, seq_len))))
        return(sequence(rlev$lengths))
    }
    else {
      out = list(idx = rep(seq_along(rlev$lengths), times = rlev$lengths), 
        seq = unlist(unname(lapply(rlev$lengths, seq_len))))
      out$lns = ave(out[[1]], out[[1]], FUN = length)
      return(out)
    }
  }
  else {
    if (!isTRUE(na.clump)) {
      vec = replace2(vec, which(x == "NA"), dedup(dg(x)[dg(x) == 
                                                          "NA"]))
    }
    vec = setNames(vec, seq_along(vec))
    lst = split(vec, factor(vec, levels = unique(vec)))
    ord = as.integer(names(unlist(unname(lst))))
    idx = rep(seq_along(lst), times = base::lengths(lst))
    out = list(idx = idx[order(ord)], seq = rleseq(idx, clump = FALSE, 
      recurs = TRUE, use.data.table = FALSE)[order(ord)])
    ## out$lns = ave(out[[1]], out[[1]], FUN = length)
    out$lns = unname(rep(base::lengths(lst), times = base::lengths(lst)))
    return(out)
  }
}

## rleseq = function(..., clump = TRUE, recurs = FALSE, na.clump = TRUE, na.ignore = FALSE,
##                   sep = paste0(" ", rand.string(length = 6), " ")) {
##     force(sep)
##     rand.string <- function(n=1, length=12)
##     {
##         randomString <- c(1:n)                  # initialize vector
##         for (i in 1:n)
##         {
##             randomString[i] <- paste(sample(c(0:9, letters, LETTERS),
##                                             length, replace=TRUE),
##                                      collapse="")
##         }
##         return(randomString)
##     }
##     if (isTRUE(na.clump))
##         paste = function(...,
##                          sep) base::paste(..., sep = sep)
##     else
##         paste = function(...,
##                          sep) base::paste(stringr::str_c(..., sep = sep))
##     lns = lengths(list(...))
##     if (!all(lns == lns[1]))
##         warning("not all vectors provided have same length")
##     fulllens = max(lns, na.rm = T)
##     vec = setNames(paste(..., sep = sep), seq_len(fulllens))
##     if (length(vec) == 0) {
##         out = list(idx = integer(0), seq = integer(0), lns = integer(0))
##         return(out)
##     }
##     ## rlev = rle(paste(as.character(vec)))
##     if (na.ignore) {
##         isnotna = which(rowSums(as.data.frame(lapply(list(...), is.na))) == 0)
##         out = list(idx = rep(NA, fulllens), seq = rep(NA, fulllens), lns = rep(NA, fulllens))
##         if (length(isnotna))
##             vec = vec[isnotna]
##         tmpout = do.call(rleseq, c(alist(... = vec),
##                                    alist(clump = clump, recurs = recurs, na.clump = na.clump, na.ignore = FALSE)))
##         ## tmpout = rleseq(..., clump = clump, recurs = recurs, na.clump = FALSE, na.ignore = FALSE)
##         for (i in seq_along(out))
##             out[[i]][isnotna] = tmpout[[i]]
##         return(out)
##     }
##     if (!isTRUE(clump)) {
##         rlev = rle(vec)
##         if (isTRUE(recurs)) {
##             return(unlist(unname(lapply(rlev$lengths, seq_len))))
##         } else {
##             out = list(
##                 idx = rep(seq_along(rlev$lengths), times = rlev$lengths),
##                 seq = unlist(unname(lapply(rlev$lengths, seq_len))))
##             out$lns = ave(out[[1]], out[[1]], FUN = length)
##             ## if (na.ignore)
##             ##     complete.cases(as.data.frame(lapply(list(...), is.na)))
##             return(out)
##         }
##     } else {
##         if (!isTRUE(na.clump)) {
##             vec = replace2(vec, which(x == "NA"), dedup(dg(x)[dg(x) == "NA"]))
##         }
##         ## vec = setNames(paste(as.character(vec)), seq_along(vec))
##         vec = setNames(vec, seq_along(vec))
##         lst = split(vec, factor(vec, levels = unique(vec)))
##         ord = as.integer(names(unlist(unname(lst))))
##         idx = rep(seq_along(lst), times = lengths(lst))
##         out = list(
##             idx = idx[order(ord)],
##             seq = rleseq(idx, clump = FALSE, recurs = TRUE)[order(ord)])
##         out$lns = ave(out[[1]], out[[1]], FUN = length)
##         return(out)
##     }
    
## }




#' @name lens
#' @title similar to lengths except gets nrows for those items that have dimensions
#'
#' @description
#' figure out length or nrows of a list
#' if there are dimensions in the list element,
#' find out the number of rows
#'
#' @param x A list
#' @return A numeric vector of lengths of each list
#' @export
lens = function(x, use.names = TRUE) {
    
    ## out = vapply(x, function(x) {
    ##     out = dim(x)[1L]
    ##     if (!is.null(out))
    ##         return(out)
    ##     else
    ##         return(length(x))
    ## }, FUN.VALUE = 1L, USE.NAMES = use.names)
    
    ## dlst = lapply(x, nrow)
    ## dlst = lapply(x, dim)
    ## out = lengths(x, use.names = use.names)
    ## ix = which(!dlst == "NULL")
    ## if (length(ix))
    ##     out[ix] = vapply(x[ix], nrow, 1L, USE.NAMES=use.names)

    out = vapply(x, NROW, FUN.VALUE  = 1L, USE.NAMES = use.names)
    return(out)
}

#' @name len
#' @title similar to length except gets nrows for those items that have dimensions
#'
#' @description
#' figure out length or nrow of an object
#' lol... this is base::NROW
#' also see base::NCOL for the alternative
#'
#' @param x an object
#' @return length or nrow of an object
#' @export
len = NROW

## len = function(x, use.names = TRUE) {
##     nr = dim(x)[1]
##     n = length(x)
##     if (!is.null(nr))
##         return(nr)
##     else
##         return(n)
## }



#' @name rg_sub
#' @title extracting substring match using regexpr
#'
#' @description
#' extract the first portion of matched substring
#'
#' @return Character vector of the regex matched portions of the input string vector
#' @export
rg_sub = function(pattern, text, ...) {
    rg = regexpr(pattern, text, ...)
    out = substr(text, rg, rg + attributes(rg)$match.length - 1)
    return(replace2(out, !nzchar(x), NA_character_) %>% trimws)
}

#' @name grg_sub
#'
#' extract all portions of matched substring
#' and collapse
#'
#' @export
grg_sub = function(pattern, text, colsep = " ", ...) {
    grg = gregexpr(pattern, text, ...)
    rg = unlist(grg)
    m.len = unlist(lapply(grg, attr, "match.length"))
    lens = lengths(grg)
    dt = data.table(text = rep(text, times = lens),
                    ix = rep(seq_along(text), times = lens),
                    iix = unlist(lapply(lens, seq_len)),
                    dummy = "out_str")
    dt[, out_str := substr(text, rg, rg + m.len - 1)]
    out = dcast.wrap(dt, lh = "ix", rh = "dummy", value.var = "out_str", fun.aggregate = function(x) paste(x, collapse = colsep))[[2]]
    return(replace2(out, !nzchar(x), NA_character_) %>% trimws)
}

#' @name dynget
#' @title modification of base::dynGet()
#'
#' @description
#' slight modification of base::dynGet()
#' minframe set to 0 to also look in global environment
#' and it's robust to using within functions
#' also takes the variable name without quotes as default
#' but can supply a character, and set px to FALSE
#'
#' @export
dynget = function(x, px = TRUE,
                  ifnotfound = stop(gettextf("%s not found", sQuote(x)),
                                    domain = NA),
                  minframe = 0L,
                  inherits = FALSE) ## modification of base::dynGet()
{
    tmp_x = as.list(match.call())$x
    if (is.name(tmp_x)) {
        if (isTRUE(px))
            x = as.character(tmp_x)
        else
            x = eval(tmp_x, parent.frame())
    }
    if (!is.character(x))
        stop("x must be a character or a name of a variable")
    n <- sys.nframe()
    myObj <- structure(list(.b = as.raw(7)), foo = 47L)
    while (n > minframe) {
        n <- n - 1L
        env <- sys.frame(n)
        r <- tryCatch(get0(x, envir = env, inherits = inherits, ifnotfound = myObj), error = function(e) return(myObj))
        if (!identical(r, myObj))
            return(r)
    }
    ifnotfound
}


#' @name dg
#' @title alias of dynget
#'
#' @description
#' convenience wrapper around dynget
#'
#' @export
dg = dynget


#' @name %inn%
#'
#' Same as %in% but keeps NA values as NA
#'
#' @return a logical vector
#' @export
`%inn%` = function(x, table) {
    vec = match(x, table, nomatch = 0L) > 0L
    vec[is.na(x)] = NA
    vec
}


#' @name dcast.count
#' @title dcast.count
#'
#' Counting up occurrences in a table while taking factor levels into account
#'
#' @return A data frame or data.table
#' @export dcast.count
dcast.count = function(tbl, lh, rh = NULL, countcol = "count", ...) {
    this.env = environment()
    if (is.null(rh))
        rh = "dummy"
    dcast.wrap(within(tbl, {dummy = this.env$countcol}), lh = lh, rh = rh, value.var = "dummy", fun.aggregate = length, fill = 0, ...)
}

#' @name dcast.count2
#' @title Counts up occurrences from a melted table
#'
#' @description
#' Counting up occurrences in a table while taking factor levels into account
#' Also allows for weighting the counts using flexible argument parsing
#' Can either provide a weight as a name of a column,
#' as values themselves, or don't provide at all, and the function looks for a
#' column named "wt" for its values
#'
#' @return A data frame or data.table
#' @export dcast.count2
dcast.count2 = function(tbl, lh, rh = NULL, countcol = "count", wt = 1, fun.aggregate = "sum", value.var = "dummy", ...) {
    suppressWarnings({tbl$dummy = NULL})
    lst.call = as.list(match.call())
    if (is.name(lst.call$fun.aggregate))
        fun.aggregate = get(as.character(lst.call$fun.aggregate))
    else if (is.call(lst.call$fun.aggregate))
        fun.aggregate
    else if (is.character(fun.aggregate))
        fun.aggregate = get(fun.aggregate)
    if ("wt" %in% names(lst.call))
        if (is.character(wt) && wt %in% colnames(tbl)) {
            expr = expression(within(tbl, {dummy = 1 * dg(wt, FALSE)}))
        } else if (is.numeric(wt)) {
            expr = expression(within(tbl, {wt = NULL; dummy = 1 * dg(wt)}))
        } else {
            stop("wt argument must be either a numeric vector, a name of a column, or a column that exists in the table")
        }
    else if (is.null(wt) || isFALSE(wt) || is.na(wt) || length(wt) == 0)
        expr = expression(within(tbl, {dummy = 1}))
    else if (!"wt" %in% names(lst.call)) {
        if ("wt" %in% colnames(tbl)) {
            message("column named \"wt\" found, will weight counts using values in this field")
        }
        expr = expression(within(tbl, {dummy = 1 * dg(wt)}))
    }
    this.env = environment()
    if (is.null(rh))
        rh = "dummy"
    out = dcast.wrap(eval(expr), lh = lh, rh = rh, value.var = value.var, fun.aggregate = fun.aggregate, fill = 0, ...)
    if ("1" %in% colnames(out))
        setnames(out, "1", countcol)
    return(out)
}





#' @name dcast.wrap
#' @title wrapper around dcast or dcast2
#'
#' A convenience wrapper around dcast to make formula generation more
#' programmatic
#'
#' @return A data frame or data.table
#' @export dcast.wrap
dcast.wrap = function (x, lh, rh, dcast.fun, ...) {
    if (missing(dcast.fun)) {
        if (inherits(x, "data.table")) 
            dcast.fun = dcast.data.table
        else dcast.fun = dcast
    }
    if (!isTRUE(is.function(dcast.fun))) 
        stop("provided dcast argument is not a function")
    dcast_form = formula(paste(paste(lh, collapse = "+"), paste(rh, 
        collapse = "+"), sep = "~"))
    return(dcast.fun(x, formula = dcast_form, ...))
}


#' @name normv
#' @title normalize a vector
#'
#' i.e. rescale to have values between 0 and 1
#'
#' @return vector
#' @export
normv = function(x) {
    (x - min(x, na.rm = T)) / (max(x, na.rm = T) - min(x, na.rm = T))
}

#' @name normv_sep
#' @title normalize a vector, treating positives and negatives separately
#'
#' i.e. rescale negatives to be between 0-0.5
#' rescale positives to be between 0.5-1
#'
#' @return vector
#' @export
normv_sep = function(x) {
    if (any(x < 0, na.rm = T))
        x[which(x < 0)] = -normv(-(x[which(x < 0)])) - 0.05
    if (any(x >= 0, na.rm = T))
        x[which(x >= 0)] = normv((x[which(x >= 0)])) + 0.05
    return(normv(x))
}


#' @name zscore
#' @title zscore a numeric vector
#'
#' @return vector
#' @export
zscore <- function(x, na.rm = F) {
    ## (x - mean(x, na.rm = na.rm)) / sd(x, na.rm = na.rm)
    mn = mean(x, na.rm = na.rm)
    stddev = sd(x, na.rm = na.rm)
    out = (x - mn) / stddev
    structure(
        out,
        mean = mn,
        stddev = stddev
    )
}


#' @name select.matrix
#'
#' wrapper to pick out rows and columns without erroring out
#'
#' @return matrix
#' @export select.matrix
select.matrix = function(x, rows = NULL, cols = NULL, int.rows = TRUE, int.cols = TRUE) {
    errcol = ""
    errrow = ""
    if (!is.null(rows)) {
        if (inherits(rows, "character")) {
            if (int.rows) {
                sel.row = intersect(rows, rownames(x))
            } else {
                sel.row = rows
            }
        } else if (!inherits(col, c("numeric", "integer"))) {
            errrow = "incorrect column specification"
        }
    } else {
        sel.row = seq_len(dim(x)[1])
    }
    if (!is.null(cols)) {
        if (inherits(cols, "character")) {
            if (int.cols) {
                sel.col = intersect(cols, colnames(x))
            } else {
                sel.row = rows
            }
        } else if (!inherits(col, c("numeric", "integer"))) {
            errcol = "incorrect column specification"
        }
    } else  {
        sel.col = seq_len(dim(x)[2])
    }
    x[sel.row, sel.col, drop = FALSE]
}



#' @name ne
#' @title "no error"
#'
#' "no error"
#'
#' @return NULL, if error
#' @export
ne = function(...) {
    return(tryCatch(..., error = function(e) NULL))
}


#' @name good.file
#' @title Does File exists and is file size greater than threshold
#'
#' Queries a set of file paths for whether the file exists AND
#' if the file is greater than a size threshold
#'
#' @param x character vector of file paths
#' @param size.thresh threshold of minimum file size
#' @return logical
#' @export good.file
good.file = function(x, size.thresh = 0) {
    (file.exists2(x) & na2false(file.size(x) > size.thresh))
}


#' @name loop_grep
#' @title wraps grep in for loop
#'
#' A wrapper around grep to identify string matches across multiple patterns
#'
#' @return integer vector of indices
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

#' @name loop_grepl
#' @title wrapper around loop_grep
#'
#' Generating a logical vector from loop_grep
#'
#' @return logical vector of all matches
#' @export
loop_grepl = function(patterns, vec_char, ignore.case = FALSE) {
    lg = logical(length(vec_char))
    ind = loop_grep(patterns, vec_char, ignore.case = ignore.case)
    lg[ind] = TRUE
    lg
}




#' @name rrrepeated
#' @title Recursively repeat a function call
#'
#' Recursively repeat a function
#' Found on stackoverflow
#'
#' @author StackOverflow
#' @return Same as .x
#' @export
rrrepeated <- function(.x, .reps = 1, .f, ...) {
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
#' @title wrapper around tryCatch - robust to parallel:: functions
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
#' @title applies dedup to colnames
#'
#' dedup the column names of a data.frame/data.table
#'
#' @return A data.table or data.frame
#' @export dedup.cols
dedup.cols = function(tbl, remove = FALSE) {
    if (remove) {
        if (!inherits(tbl, "data.table"))
            return(tbl[, match(unique(colnames(tbl)), colnames(tbl))])
        else
            return(tbl[, match(unique(colnames(tbl)), colnames(tbl)), with = FALSE])
    } else {
            colnames(tbl) = dedup(colnames(tbl))
            return(tbl)
    }
}



#' @name pinch.frac
#' @title pinch a vector
#'
#' A convenience function to transform proportions.
#' Useful for beta regression (library(betareg))
#'
#' @return A vector
#' @export
pinch.frac = function(x, fmin = 0.01, fmax = 0.99) {
    pmax(pmin(x, fmax), fmin)
}

#' @name pinch
#' @title pinch a vector
#'
#' A convenience function to transform proportions.
#' Useful for beta regression (library(betareg))
#' Can be used on any numeric values to squeeze between
#' an interval.
#'
#' @return A vector
#' @export
pinch = function(x, fmin = 0.01, fmax = 0.99) {
    pmax(pmin(x, fmax), fmin)
}


#' @name binom.conf
#' @title Get confidence intervals around fractions
#'
#' @description
#' A convenience function to get confidence intervals around
#' proportions. To be used with gbar.error
#'
#' @return A vector
#' @export binom.conf
binom.conf = function(n, tot, alpha = 0.025, tol = 1e-8) {
    suppressWarnings({
        conf.low = qbinom(p = (1 - (alpha)), size = tot, prob = n / tot, lower.tail = FALSE) / tot
        conf.high= qbinom(p = (1 - (alpha)), size = tot, prob = n / tot, lower.tail = TRUE) / tot
    })
    dt = data.table(frac = n / (tot + tol),
                    conf.low = replace2(conf.low, is.na(x), 0),
                    conf.high = replace2(conf.high, is.na(x), 0))
    return(dt)
}




#' @name getdat
#' @title a method to get the "data" argument from a with/within expression
#'
#' to be used within "with()" within the expression
#'
#' @return data.frame/data.table
#' @export
getdat = function(n = 0L) { ## to be used within "with()" expr
    tmpfun = function() {
        current.n = sys.nframe()
        myObj <- structure(list(.b = as.raw(7)), foo = 47L)
        while (current.n >= 0) {
            out = tryCatch(mget("envir", sys.frame(current.n), mode = "list", ifnotfound = myObj), error = function(e) myObj)
            if (inherits(out[[1]], "data.table"))
                return(out[[1]])
            current.n = current.n - 1
        }
    }
    pf = parent.frame(3 + n)
    if (identical(environmentName(pf), "R_GlobalEnv"))
        return(invisible(NULL))
    if ("data" %in% names(pf))
        data = get("data", pf)
    else if ("envir" %in% names(parent.frame(2)) &&
             inherits(tmpfun(), "data.table")) ## recent addition
        data = tmpfun()
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
#' @title alias for getdat
#'
#' @description
#' alias for getdat function
#'
#' @export
gd = getdat

#' @name getdat2
#' @title getdat2
#'
#' @description
#' another function to use inside the expression argument of "with/within" family
#' to grab the enclosing data environment
#'
#' @export
getdat2 = function(nm = "data") { ## to be used within "with()" expr
    this.environment = environment()
    return(dg(nm, F))
}


#' @name cenv
#' @title concatenate environments
#'
#' @description
#' to be able to call a function within a function and access variables
#' use:
#' ev = "bla"
#' this.fun = function() anon()
#' datatable = data.table()
#' anon = function() {
#'     with(cenv(datatable), {print(ev)})
#' }
#' this.fun()
#'
#' @param env can be a data.frame/table, or list, or environment
#' @export
cenv = function(env = environment()) {
    expr_193659793_155174963 = as.expression(substitute(expr_6000525395_6907698684, env = env))
    if (is.environment(env))
        rm(expr_6000525395_6907698684, envir = env)
    thisenv = c(list(data = env), as.list(env))
    fms = sys.frames()
    for (i in rev(seq_along(fms))) {
        suppressWarnings(rm(expr_6000525395_6907698684, envir = fms[[i]]))
        thisenv = c(thisenv, tryCatch(as.list(fms[[i]]), error = function(e) NULL))
    }
    thisenv = c(list(expr_193659793_155174963 = expr_193659793_155174963), thisenv, as.list(globalenv()))
    return(thisenv)
}

## cenv = function(env = parent.frame()) {
##     thisenv = c(list(data = env), as.list(env))
##     fms = sys.frames()
##     for (i in rev(seq_along(fms))) {
##         thisenv = c(thisenv, tryCatch(as.list(fms[[i]]), error = function(e) NULL))
##     }
##     return(thisenv)
## }


#' @name main
#' @title wrapper around cenv
#'
#' @description
#' to be able to main
#'
#' @param expr expression to evaluate
#' @param env environment
#' @param return logical
#' @export
main = function(expr_6000525395_6907698684, return = F) {
    env = environment()
    env.lst = cenv(env = env)
    out = with(env.lst, {
        tryCatch(expr_6000525395_6907698684, error = function(e) NULL);
        eval(expr_193659793_155174963, env.lst)
    })
    if (return)
        return(out)
    else
        return(NULL)
}

#' @name errr
#' @title turn on verbose error tracing through call stack
#'
#'
#' @export
errr = function(x = 2) {
    er = options()$error
    if (is.null(er) || !missing(x)) {
        message("error traceback on, traceback level set to ", x)
        options(error = function() { traceback(x); print("ERROR"); })
    } else {
        message("error traceback off")
        options(error = NULL)
    }
}


#' @name g2
#' @title alias for getdat2
#'
#' @description
#' alias for getdat2 function
#'
#' @export
g2 = getdat2

#' @name g
#' @title alias for getdat2
#'
#' @description
#' alias for getdat2 function
#'
#' @export
g = getdat2

#' @name gx
#' @title alias for getdat2(nm = "x")
#'
#' @description
#' alias for getdat2(nm = "x")
#'
#' @export
gx = function(nm = "x") eval.parent(getdat2(nm = nm))

#' @name withx
#' @title withx
#'
#' to be used for quick interactive programming
#' withx(toolongtotypemeagain, x * sum(x))
#'
#' @export
withx <- function(x, expr) {
    env = environment()
    senv = parent.frame()
    suppressWarnings(eval(substitute(expr), env, enclos = senv))
}

#' @name withv
#' @title withv
#'
#' to be used for quick interactive programming
#' withv(toolongtotypemeagain, x * sum(x))
#'
#' @export
withv = function(x, expr) {
    env = environment()
    senv = stackenv2(parent.frame())
    suppressWarnings(eval(substitute(expr), env, enclos = senv))
}

#' @name stackenv
#' @title stackenv
#'
#' 
#'
#' @export
stackenv = function(env = environment(), onlyanc = TRUE, asenv = TRUE) {
    fms = sys.frames()
    thisenv = as.list(env)
    ## these = rev(seq_along(fms))
    these = rev(seq_len(sys.nframe()))
    if (onlyanc) these = these[-1]
    for (i in these) {
        thisenv = c(thisenv, tryCatch(as.list(fms[[i]]), error = function(e) NULL))
    }
    thisenv = c(thisenv, as.list(globalenv()))
    if (asenv)
        return(as.environment(thisenv))
    else
        return(thisenv)
}


#' @name stackenv2
#' @title stackenv2
#'
#' 
#'
#' @export
stackenv2 = function(overwrite = FALSE, onlyanc = TRUE, verbose = FALSE) {
    fms = sys.frames()
    thisenv = new.env()
    parent.env(thisenv) = parent.frame()
    ## these = rev(seq_along(fms))
    these = rev(seq_len(sys.nframe()))
    if (onlyanc) these = these[-1]
    for (i in these) {
        thisenv = suppressWarnings(appendEnv(thisenv, tryCatch(fms[[i]], error = function(e) NULL), overwrite = overwrite))
        if (verbose) {
            message("frame i: ", i)
            print(ls(thisenv))
        }
    }
    thisenv = suppressWarnings(appendEnv(thisenv, globalenv(), overwrite = overwrite))
    return(thisenv)
}

#' @name appendEnv
#' @title appendEnv
#'
#' 
#' @author qedqed from Stackoverflow
#' @export
appendEnv = function(e1, e2 = NULL, overwrite = FALSE) {
    if (is.null(e2))
        return(e1)
    e1name = deparse(substitute(e1))
    e2name = deparse(substitute(e2))
    listE1 = ls(e1, sorted = FALSE)
    listE2 = ls(e2, sorted = FALSE)
    rstring = rand.string()
    for(v in listE2) {
        if (v %in% listE1) {
            msg = sprintf("Variable %s is in e1, too!", v)
            if (!isTRUE(overwrite)) {
                paste0(msg, " ... skipping ...")
                next
            }
            warning(msg)
        }
        this = tryCatch(get0(v, envir = e2, inherits = FALSE,
                             ifnotfound = structure("missing", class = rstring)),
                        error = function(e) structure("missing", class = rstring))
        if (!class(this)[1] == rstring)
            e1[[v]] = e2[[v]]
    }
    return(e1)
}

#' @name wv
#' @title alias for withv
#'
#' to be used for quick interactive programming
#' withv(toolongtotypemeagain, x * sum(x))
#'
#' @export
wv = withv


with2 = function(data, expr, ...) {
    data = data
    eval(substitute(expr), data)
}

#' @name file.info2
#' @title file.info2
#'
#' @description
#' A more robust file.info2 that removes any paths that do not exist
#'
#' @return data.frame/data.table
#' @export
file.info2 = function(fn, col = NULL, include.all = FALSE) {
    lst.call = as.list(match.call())
    if (!"col" %in% names(lst.call) & grepl("[/$]", base::toString(substitute(fn))))
        col = "path"
    if (is.null(col)) col = as.character(substitute(fn))
    fif = file.info(unique(subset2(fn, file.exists2(x)))) %>% rownames_to_column(col) %>% as.data.table
    if (include.all) {
        fif = merge(setnames(data.table(fn), col)[, tmp.ord := seq_along(fn)],
                    fif,
                    by = col, all = TRUE)[order(tmp.ord)][, tmp.ord := NULL]
    }
    fif
}


#' @name subset2
#' @title function to subset on a variable by using "x" as surrogate variable in expression
#'
#' convenience function to subset without having to type excessively
#' if the variable is arrived at through nested functions or long
#' variable names
#'
#' @author Kevin Hadi
#' @export
subset2 = function(x, sub.expr, ...) {
    if (!missing(sub.expr)) {
        this.sub = eval(as.list(match.call())$sub.expr)
        if (is.numeric(this.sub)) {
            if (any(this.sub %% 1))
                stop("subset must be integer")
            if (!is.null(dim(x))) {
                if (!all(this.sub %in% seq_len(nrow(x))))
                    stop("subset must be indexed within rows of x")
                else
                    this.sub = replace(logical(nrow(x)), this.sub, TRUE)
            } else {
                if (!all(this.sub %in% seq_along(x)))
                    stop("subset must be indexed within x")
                else
                    this.sub = replace(logical(length(x)), this.sub, TRUE)
            }
        }
    } else if (missing(sub.expr)) {
        if (!is.null(dim(x)))
            ## this.sub = seq_len(nrow(x))
            this.sub = logical(nrow(x)) | TRUE
        else
            this.sub = logical(length(x)) | TRUE
            ## this.sub = seq_along(x)
    }
    subset(x, this.sub, ...)
}

#' @name ss
#' @title same as subset2
#'
#' convenience function to subset without having to type excessively
#' if the variable is arrived at through nested functions or long
#' variable names
#'
#' @author Kevin Hadi
#' @export
ss = subset2


#' @name replace2
#' @title function to replace elements of vector, can use "x" as surrogate variable in expression
#'
#' @description
#'
#' convenience function to replace without having to type excessively
#' if the variable is arrived at through nested functions or long
#' variable names
#'
#' @export
replace2 = function(x, repl.expr, values) {
    lst.call = as.list(match.call())
    if ("list"  == as.character(lst.call$repl.expr)[1]) {
        exprs = as.list(lst.call$repl.expr)[-1]
        length(values)
        if (!length(exprs) == length(values) && !length(values) == 1)
            stop("list of expressions must be the same length as values")
        for (i in seq_along(exprs)) {
            if (length(values) > 1)
                x[eval(exprs[[i]])] = values[[i]]
            else
                x[eval(exprs[[i]])] = values
        }
        return(x)
    } else {
        this.repl = eval(lst.call$repl.expr)
        if (inherits(this.repl, "list")) {
            if (!length(this.repl) == length(values) && !length(values) == 1)
                stop("list provided must be the same length as values")
            else {
                for (i in seq_along(this.repl)) {
                    if (length(values) > 1)
                        x[eval(this.repl[[i]])] = values[[i]]
                    else
                        x[eval(this.repl[[i]])] = values
                }
                return(x)
            }
        } else {
            this.repl = eval(lst.call$repl.expr)
            return(replace(x, this.repl, values = values))
        }
    }
}

#' @name replace_na
#' @title replace NAs
#'
#' replace_na
#'
#' @export
replace_na = function(data, replace) {
    return(replace(data, is.na(data), replace))
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

#' @name ave3
#' @title modification of ave
#'
#' slight update of ave
#'
#' @export
ave3 = function (x, ..., FUN = mean) 
{
    if (missing(...)) {
        x[] = FUN(x)
    }
    else {
        rl = rleseq(..., clump = TRUE)
        ## g = interaction2(...)
        lidx = .Internal(split(seq_along(x), factor(rl$idx)))
        spl = split(x, rl$idx)
        ## spl = split(x, g)
        lst = lapply(spl, FUN)
        ## return(rep(unlist(lst), lengths(lidx))[unlist(lidx)])
        return(unlist(rep(lst, lengths(lidx) - lengths(lst) + 1))[order(unlist(lidx))])
    }
    x
}


#' @name aved
#' @title modification of ave
#'
#' slight update of ave
#'
#' @export
aved = function(..., FUN = length, drop = TRUE) {
  ## browser()
  ## ddd = as.list(match.call(expand.dots = FALSE)[["..."]])
  ## if (length(ddd) > 1) {
  ##   do.call(function(...) interaction2(..., drop = drop), ddd, envir = parent.frame())
  ## }
  g = interaction2(..., drop = drop)
  lidx = .Internal(split(seq_along(g), g))
  spl = split(g, g)
  lst = lapply(spl, FUN)
  return(rep(unlist(lst), lengths(lidx))[unlist(lidx)])
}





#' @name rematch
#' @title rematch
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
#' @title make list of matches and nonmatches
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
#' @title file.mat.exists
#'
#' run file.exists2 on columns of a table
#'
#' @export file.mat.exists
file.mat.exists = function(x, rm_col1 = FALSE) {
    matrify(x, rm_col1 = rm_col1) %>% {setRownames(apply(., 2, file.exists2), rownames(.))}
}


#' @name `%nin%`
#' @title not %in%
#'
#' Not match
#'
#' @export
`%nin%` = function (x, table)
{
    match(x, table, nomatch = 0L) == 0L
}

#' @name f2int
#' @title factor to integer
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
#' @title modified system2
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

#' @name is.empty
#' @title test if object is empty
#'
#' @author Kevin Hadi
#' @export
is.empty = function(x) {
    if (!is.null(dim(x))) {
        dim(x)[1] == 0
    } else {
        length(x) == 0 || is.null(x)
    }
}

#' @name min.col.narm
#' @title minimum column per row (removing NA)
#'
#' Return the index of the minimum column per row while removing NA
#'
#' @author stackoverflow
#' @export min.col.narm
min.col.narm = function(mat, ties.method = "first") {
    ok = max.col(-replace(mat, is.na(mat), Inf), ties.method=ties.method) * NA ^ !rowSums(!is.na(mat))
    return(ok)
}

#' @name max.col.narm
#' @title maximum column per row (removing NA)
#'
#'
#' Return the index of the maximum column per row while removing NA
#'
#' @author Stackoverflow
#' @export max.col.narm
max.col.narm = function(mat, ties.method = "first") {
    ok = max.col(replace(mat, is.na(mat), -Inf), ties.method=ties.method) * NA ^ !rowSums(!is.na(mat))
    return(ok)
}


#' @name table2
#' @title wrapper around table(), show NA counts if there are any
#'
#' Convenience wrapper around table to show NA if there are any
#'
#' @export
table2 = function(...) {
    return(table(..., useNA = "ifany"))
}

#' @name table3
#' @title wrapper around table, always show NA counts
#'
#' Convenience function to always show NA counts
#'
#' @export
table3 = function(...) {
    return(table(..., useNA = "always"))
}

#' @name dir2
#' @title dir with full grep
#'
#'
#' @author Kevin Hadi
#' @export
dir2 = function(path = ".", pattern = NULL, all.files = FALSE, full.names = FALSE,
    recursive = FALSE, ignore.case = FALSE, include.dirs = FALSE,
    no.. = FALSE, ...) {
    paths = dir(path = path, all.files = all.files,
                full.names = full.names, recursive = recursive,
                ignore.case = ignore.case, include.dirs = include.dirs,
        no.. = no..)
    if (!is.null(pattern))
        paths = grep(pattern = pattern, x = paths, value = TRUE, ...)
    return(paths)
}

#' @name dig_dir
#' @title dig into a file path's directory
#'
#' Convenience wrapper around dir() to pull out files from the same
#' directory of a given file.
#'
#' @author Kevin Hadi
#' @export
dig_dir = function (x, pattern = NULL, full.names = TRUE, mc.cores = 1,
    unlist = TRUE, do_dirname = TRUE, ...)
{
    if (is.null(pattern)) {
        pattern = list(NULL)
    }
    if (isTRUE(do_dirname))
        input = dirname(x)
    else
        input = x
    lst = lst.empty2na(
        mcMap(
            function(m.x, m.pattern, ...) {
                dir(
                    path = m.x,
                    pattern = m.pattern,
                    full.names = full.names,
                    ...
                )
            },
            input,
            pattern,
            mc.cores = mc.cores,
            MoreArgs = list(...)
        )
    )
    if (unlist == TRUE) {
        if (!is.null(names(lst))) {
            en = eNROW(lst)
            nm = rep(names(lst), en)
        }
        ul = unlist(lst)
        names(ul) = nm
        return(ul)
    }
    return(lst)
}

## dig_dir = function (x, pattern = NULL, full.names = TRUE, mc.cores = 1,
##     unlist = TRUE, ...)
## {
##     if (is.null(pattern)) {
##         pattern = list(NULL)
##     }
##     if (unlist == TRUE) {
##         unlist(lst.empty2na(mcMap(function(m.x, m.pattern, ...) {
##             dir(path = m.x, pattern = m.pattern, full.names = full.names, ...)
##         }, dirname(x), pattern, mc.cores = mc.cores, MoreArgs = list(...))))
##     }
##     else {
##         lst.empty2na(mcMap(function(m.x, m.pattern, ...) {
##             dir(path = m.x, pattern = m.pattern, full.names = full.names, ...)
##         }, dirname(x), pattern, mc.cores = mc.cores, MoreArgs = list(...)))
##     }
## }


#' @name dig_dir2
#' @title dig into a file path's directory
#'
#' Convenience wrapper around dir2() to pull out files from the same
#' directory of a given file.
#'
#' @author Kevin Hadi
#' @export
dig_dir2 = function (x, pattern = NULL, full.names = TRUE, mc.cores = 1,
    unlist = TRUE,  do_dirname = TRUE, ...)
{
    if (is.null(pattern)) {
        pattern = list(NULL)
    }
    if (isTRUE(do_dirname))
        input = dirname(x)
    else
        input = x
    lst = lst.empty2na(
        mcMap(
            function(m.x, m.pattern, ...) {
                dir2(
                    path = m.x,
                    pattern = m.pattern,
                    full.names = full.names,
                    ...
                )
            },
            input,
            pattern,
            mc.cores = mc.cores,
            MoreArgs = list(...)
        )
    )
    if (unlist == TRUE) {
        if (!is.null(names(lst))) {
            en = eNROW(lst)
            nm = rep(names(lst), en)
        }
        ul = unlist(lst)
        names(ul) = nm
        return(ul)
    }
    return(lst)
}

## dig_dir2 = function (x, pattern = NULL, full.names = TRUE, mc.cores = 1,
##     unlist = TRUE, ...)
## {
##     if (is.null(pattern)) {
##         pattern = list(NULL)
##     }
##     if (unlist == TRUE) {
##         unlist(lst.empty2na(mcMap(function(m.x, m.pattern, ...) {
##             dir2(path = m.x, pattern = m.pattern, full.names = full.names, ...)
##         }, dirname(x), pattern, mc.cores = mc.cores, MoreArgs = list(...))))
##     }
##     else {
##         lst.empty2na(mcMap(function(m.x, m.pattern, ...) {
##             dir2(path = m.x, pattern = m.pattern, full.names = full.names, ...)
##         }, dirname(x), pattern, mc.cores = mc.cores, MoreArgs = list(...)))
##     }
## }



#' @name stack.dt
#' @title collapse a named list of vectors into a data.table
#'
#' Collapse a named list with vectors as each element into a data.table
#'
#' @export stack.dt
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
#' @title make_chunks
#'
#' @description
#' Create chunks from a vector with a certain number of elements per chunk
#' OR create a certain number of chunks from a vector
#'
#' @return A list
#' @export
make_chunks = function(vec, n = 100, max_per_chunk = TRUE, num_chunk = !max_per_chunk, seed = 10) {
    set.seed(seed)
    lst.call = as.list(match.call())
    if (!is.null(lst.call$num_chunk) && is.null(lst.call$max_per_chunk)) {
        max_per_chunk = !eval(lst.call$num_chunk)
    }
    if ((isTRUE(max_per_chunk) & isTRUE(num_chunk)) ||
        (isFALSE(max_per_chunk) & isFALSE(num_chunk)) ||
        (!is.logical(max_per_chunk) & !is.logical(num_chunk)))
        stop("select either max_per_chunk OR num_chunk to be TRUE")
    if (max_per_chunk)
        splitter = ceiling(length(vec)) / n
    if (num_chunk)
        splitter = n
    ## ind = parallel::splitIndices(length(vec), ceiling(length(vec) / max_per_chunk))
    ind = parallel::splitIndices(length(vec), splitter)
    split(vec, rep(seq_along(ind), times = base::lengths(ind)))
}



#' @name globasn
#' @title assign an object to global environment
#'
#' @description
#' ONLY USE IF YOU KNOW WHAT YOU ARE DOING
#' This function forces assignment of a variable/function
#' to the global environment, or an environment of your choosing
#'
#' @param obj The object to assign to the global environment
#' @param var Optional name of variable, specified as string
#' @return either NULL or the object being assigned
#' @export
globasn = function(obj, var = NULL, return_obj = TRUE, envir = .GlobalEnv, verbose = TRUE, vareval = F)
{
    var = as.list(match.call())$var
    if (is.null(var)) {
        globx = as.character(substitute(obj))
    } else {
        if (is.name(var)) {
            if (isFALSE(vareval))
                var = as.character(var)
            else
                var = eval(var, parent.frame())
        } else if (!is.character(var)) {
            stop("var must be coercible to a character")
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


#' @name reassign
#' @title reassign elements of named list into environment
#'
#' ONLY USE IF YOU KNOW WHAT YOU ARE DOING
#' this function takes a list of named objects and assigns them to the calling environment
#'
#' @param variables_lst A named list of variables
#' @return the input list
#' @export
reassign = function(variables_lst, calling_env = parent.frame()) {
    for (i in 1:length(variables_lst)) {
        message("variable assigned to: ", names(variables_lst[i]))
        assign(names(variables_lst[i]), variables_lst[[i]], envir = calling_env)
    }
    invisible(variables_lst)
}



##############################
##############################
############################## multiROC helpers
##############################
##############################

#' @name classystat
#' @title calculates various scores from actual classes and predicted classes
#'
#' Scores calculated from a classification task with known true labels:
#' precision, recall
#' F1, mean f1, weighted mean f1, mean precision,
#' mean recall, weighted mean recall, accuracy
#'
#' @param real true class labels
#' @param pred predicted class labels
#' @return a list of various statistics for clasification task
#' @export
classystat= function(real, pred) {
  if (!inherits(real, "factor"))
    real = factor(real)
  if (!inherits(pred, "factor"))
    pred = factor(pred)
  if (length(setdiff(levels(real), levels(pred))) > 0)
    stop("real and pred must have matching levels!")
  if (! all(levels(real) == levels(pred)))
    pred = factor(pred, levels = levels(real))
  rp = (table2(real = real, pred = pred) %>% melt %>% asdt)
  acc = with(rp, {
    sum(value[real == pred]) / sum(value)
  })
  prec = rp[, sum(value[real == pred]) / sum(value), by = pred]
  reca = rp[, sum(value[real == pred]) / sum(value), by = real]
  tots = rp[, sum(value), by = real]$V1
  grandtot = rp[, sum(value)]
  prec[, wV1 := tots * V1 / grandtot]
  reca[, wV1 := tots * V1 / grandtot]
  aggprec = mean(prec$V1)
  aggreca = mean(reca$V1)
  aggwprec = sum(prec$wV1)
  aggwreca = sum(reca$wV1)
  f1 = setNames(2 * (prec$V1 * reca$V1) / (prec$V1 + reca$V1), prec[[1]])
  aggf1 = mean(f1)
  wf1 = setNames(tots * f1 / grandtot, prec[[1]])
  ## weighted aggregate F1
  waggf1 = sum(wf1)
  list(
    total_true = rp[, sum(value), by =real][, setNames(V1, real)],
    precision = with(prec, setNames(V1, pred)),
    recall = with(reca, setNames(V1, real)),
    f1 = f1,
    mean_precision = aggprec,
    mean_recall = aggreca,
    weighted_mean_precision = aggwprec,
    weighted_mean_recall = aggwreca,
    mean_f1 = aggf1,
    weighted_mean_f1 = waggf1,
    accuracy = acc
  )
}

#' @name mroclab
#' @title create one hot table of labels for multiROC
#'
#' @param y factor
#' @return the input list
#' @export
mroclab = function(y) {
    if (!inherits(y, "factor"))
        stop("y must be a factor")
    lbl = mltools::one_hot(data.table(y))
    colnm = gsub("y_", "", colnames(lbl))
    lbl = setColnames(lbl, paste0(colnm, " _true"))
    lbl
}

#' @name mrocpred
#' @title format predicted scores for multiROC
#'
#' from predict(...)
#'
#' @param prd0 a matrix of prediction scores
#' @return A prediction
#' @export
mrocpred = function(prd0, nm = "agg") {
    prd = asdf(prd0)
    prd = setColnames(prd, paste0(colnames(prd), " _pred_", nm))
    prd
}


#' @name mrocdat
#' @title create a data frame for ggplotting multiroc
#'
#'
#'
#' @param lbl output from mroclab
#' @param prd output from mrocpred
#' @return A data.frame that can be fed into ggplot
#' @export
mrocdat = function(lbl, prd) {
  prd00 = rbind(0, asm(mrocpred(prd)))
  mg = setcols(with((melt(prd00)), g2()[order(Var2, value),]), c("Var2", "value"), c("Group", "prd"))[, c("Group", "prd"), drop = F]
  cb = cbind(lbl, prd)
  roc_res = multiROC::multi_roc(cb, force_diag = T)
  plot_roc_df <- multiROC::plot_roc_data(roc_res)
  gdat = asdt(plot_roc_df)[Group %nin% c("Macro", "Micro")][, prd := mg$prd]
  gdat[, Group := trimws(Group)]
  withAutoprint(gdat, echo = F)$value
}


#' @name ggmroc
#' @title ggplot for multiROC
#'
#' helper function for outputting ggplot
#'
#' @param lbl output from mroclab
#' @param prd output from mrocpred
#' @return A data.frame that can be fed into ggplot
#' @export
ggmroc <- function (gdat, palette = "Moonrise2", color.field = "Group", 
    linetype.field = "Method", roc.size = 1) 
{
    gdat$linetype.field = gdat[[linetype.field]]
    gdat$color.field = gdat[[color.field]]
    g = with(gdat, {
        ggplot(g2(), aes(x = 1 - Specificity, y = Sensitivity)) + 
            geom_path(aes(color = color.field, linetype = linetype.field), 
                size = roc.size) + geom_segment(aes(x = 0, y = 0, 
            xend = 1, yend = 1), colour = "grey", linetype = "dotdash") + 
            scale_colour_manual(values = skitools::brewer.master(length(unique(color.field)), 
                wes = T, palette = palette)) + theme_bw() + theme(plot.title = element_text(hjust = 0.5), 
            legend.justification = c(1, 0), legend.position = c(0.95, 
                0.05), legend.title = element_blank(), legend.background = element_rect(fill = NULL, 
                size = 0.5, linetype = "solid", colour = "black"))
    })
    return(g)
}



##############################
##############################
############################## end multiROC helpers
##############################
##############################



##############################
############################## factor helpers / forcats wrappers
##############################

#' @name levelsinuse
#' @title get the levels in use in a factor
#'
#'
#'
#' @return the levels of a factor that are represented in the factor
#' @export
levelsinuse = function(fct) {
    levels(fct)[tabulate(fct, nbins = length(levels(fct))) != 0]
}

#' @name refactor
#' @title refactor
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


#' @name sstat
#' @title kevin's implementation of sstat
#'
#'
#' @description
#' slow, but has complete names 
#'
#' @return character
#' @export
sstat <- function (full = FALSE, numslots = TRUE, resources = T) 
{
    asp = "username,groupname,state,name,jobid,associd"
    if (resources) {
        asp = paste0(asp, ",", "timelimit,timeused,submittime,starttime,endtime,eligibletime,minmemory,numcpus,numnodes,priority,nice,reason,reboot")
    }
    cmd = paste(
        "squeue -O",
        paste(
            paste0(unlist(strsplit(asp, ",")),
                   ":2000"),
            collapse = ","),
        "|",
        "sed 's/[[:space:]]\\{2,\\}/\\t/g'"
    )
    p = pipe(cmd)
    res = readLines(p)
    close(p)
    header = res[1]
    res = res[-1]
    nms = strsplit(header, "\t")[[1]] %>% tolower
    out = setnames(
        do.call(data.table, data.table::tstrsplit(res, "\t")),
        nms
    )
    out$state = factor(out$state, unique(c(out$state, "RUNNING"))) %>% 
        relevel("RUNNING")
    if (!full) {
        if (numslots) 
            out = dcast.data.table(out[, sum(as.numeric(cpus)), 
                by = .(user, state)], user ~ state, fill = 0, 
                value.var = "V1")[rev(order(RUNNING)), ]
        else out = dcast.data.table(out[, .N, by = .(user, state)], 
            user ~ state, fill = 0, value.var = "N")[rev(order(RUNNING)), 
            ]
    }
    return(out)
}



#' @name dirfind
#' @title Dig into outdir of flow job
#'
#'
#' @description
#' look at output directory of flow job
#'
#' @return character
#' @export
dirfind <- function(job, pattern, full.names = TRUE, recursive = TRUE) {
  dir2(outdir(job), pattern, full.names = full.names, recursive = recursive)
}


#' @name dig_job
#' @title Dig into Flow Job that generated an output
#'
#'
#' @description
#' takes a path of an output of a flow job and looks for
#' Job.rds and reads in Job object
#'
#' @return Job
#' @export
dig_job <- function(x, readin = TRUE, get_inputs = FALSE) {
  d = dig_dir(x, "Job.rds")
  if (readin) {
    d = readRDS(d)
    if (get_inputs)
      d = inputs(d)
    return(d)
  } else {
    return(d)
  }
}

#' @name digjob
#' @title Dig into Flow Job that generated an output
#'
#'
#' @description
#' takes a path of an output of a flow job and looks for
#' Job.rds and reads in Job object
#'
#' @return Job
#' @export
digjob <- dig_job

#' @name diginjob
#' @title Dig into inputs Flow Job that generated an output
#'
#'
#' @description
#' takes a path of an output of a flow job and looks for
#' Job.rds and reads in input
#'
#' @return data.table 
#' @export
diginjob <- function(x) {
  dig_job(x, readin = TRUE, get_inputs = TRUE)
}


#' @name output_cols
#' @title output the union columns from a Flow output
#'
#'
#'
#' @return character vector of all output columns to be expected
#' @export
output_cols = function(x, mc.cores = 1) {
    lst = mclapply(dig_dir(x, "Job.rds"), function(x) {
        op = outputs(readRDS(x))
        cn = setdiff(colnames(op), key(op))
        cn
    }, mc.cores = mc.cores)
    Reduce(f = union, lst)
}


#' @name viewtask
#' @title convert job task to table
#'
#' Parse task from Flow Job object, character task file,
#' or Flow Task object
#'
#'
#' @return A Flow job object
#' @export
viewtask = function(jb, arglst = c("name", "arg", "default")) {
  ifun = function(x, arglst = arglst) {
    unlist(lst.emptychar2na(lst.zerochar2empty(lapply(arglst, function(y)
      tryCatch((slot(x, y)), error = function(e) NA_character_)))))
  }
  if (inherits(jb, "Job"))
    obj = jb@task
  else if (inherits(jb, "Task"))
    obj = jb
  else if (inherits(jb, "character"))
    obj = Task(jb)
  as.data.table(data.table::transpose(lapply(obj@args, ifun, arglst = arglst)))
}


#' @name idj
#' @title idj
#'
#' Match up ids to a job
#'
#' @return A Flow job object
#' @export
idj = function(x, these.ids) {
    x[match(these.ids, ids(x))]
}

#' @name reset.job
#' @title reset.job
#'
#' Reset a job with different params
#'
#' @return A Flow job object
#' @export reset.job
reset.job = function(x, ..., i = NULL, rootdir = x@rootdir, jb.mem = x@runinfo$mem, jb.cores = x@runinfo$cores, jb.time = x@runinfo$time, update_cores = 1, task = NULL) {
    if (!inherits(x, "Job")) stop ("x must be a Flow Job object")
    if (is.null(task))
        usetask = x@task
    else if (is.character(task) || inherits(task, "Task"))
        usetask = task
    args = list(...)
    new.ent = copy(entities(x))
    if (!is.null(i)) {
        jb.mem = replace(x@runinfo$mem, i, jb.mem)
        jb.cores = replace(x@runinfo$cores, i, jb.cores)
    }
    tsk = viewtask(usetask)
    ## if (!all(names(args) %in% colnames(new.ent)))
    if (!all(names(args) %in% colnames(new.ent)) && !names(args) %in% viewtask(usetask)$V2)
        stop("adding additional column to entities... this function is just for resetting with new arguments")
    for (j in seq_along(args))
    {
        data.table::set(new.ent, i = i, j = names(args)[j], value = args[[j]])
    }
    these.forms = formals(body(findMethods("initialize")$Job@.Data)[[2]][[3]])
    if ("time" %in% names(these.forms)) {
        if ("update_cores" %in% names(these.forms))
            jb = Job(usetask, new.ent, rootdir = rootdir, mem = jb.mem, time = jb.time, cores = jb.cores, update_cores = update_cores)
        else
            jb = Job(usetask, new.ent, rootdir = rootdir, mem = jb.mem, time = jb.time, cores = jb.cores)
    } else {
        if ("update_cores" %in% names(these.forms))
            jb = Job(usetask, new.ent, rootdir = rootdir, mem = jb.mem, cores = jb.cores, update_cores = update_cores)
        else
            jb = Job(usetask, new.ent, rootdir = rootdir, mem = jb.mem, cores = jb.cores)
    }
    return(jb)
}

#' @name getcache
#' @title getcache
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

#' @name get_accuracy
#' @title calculate accuracy based on contingency table
#'
#' 
#'
#' @return numeric
#' @export
get_accuracy <- function(confus_mat) {
    correct = diag(confus_mat)
    sum(correct) / sum(off_diag(confus_mat), correct)
}

#' @name off_diag
#' @title get off diagonal values
#'
#' @description
#' used for get_accuracy()
#'
#' @return
#' @export
off_diag <- function(x) {
    diag(x) = NA
    as.vector(x) %>% na.omit
}

#' @name glm.nb2
#' @title glm.nb2
#'
#' Run a negative binomial regression.
#' If it fails, run a poisson regression
#'
#' @return A GLM model
#' @export glm.nb2
glm.nb2 = function(...) {
    mod = tryCatch(MASS::glm.nb(...), error = function(e) {
        warning("glm.nb broke... using poisson")
        return(stats::glm(..., family = "poisson"))
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
#' @title tabular summary of glm model coefficients
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

#' @name lbl.gg
#' @title convenience function to label the nodes and edges of gGraph
#'
#' @description
#' for gTrack visualization
#'
#' @export lbl.gg
lbl.gg = function(gg, do = T) {
    if (do) {
        gg2 = copy3(gg)
        gg2$nodes$mark(labels = gg2$nodes$dt$snode.id)
        gg2$edges[type == "ALT"]$mark(labels = gg2$edges[type == "ALT"]$dt$edge.id)
        return(gg2)
    } else {
        return(gg)
    }
}


#' @name gt.each
#' @title convenience function to plot each element separately
#'
#' useful for grangeslists and gwalks to plot each one on a separate track
#' good for visualization
#'
#' @return gTrack
#' @export gt.each
gt.each = function(gr) {
    ix = seq_along(gr)
    gtrackfun = function(x, ...) {
        if (inherits(x, c("gWalk", "gGraph")))
            return(x$gtrack(...))
        else if (inherits(x, c("GRanges", "GRangesList")))
            return(gTrack(x, ...))
    }
    fx = function(i) {
        this = gr[i]
        gtrackfun(this, name = i)
    }
    lapply(ix, fx) %>% dodo.call2(FUN = c)
}

#' @name grab_cov
#' @title convenience function to pull out coverage data from table
#'
#'
#' @return GRanges of coverage data
#' @export
grabcov = function(pairs, id = NULL, field = "decomposed_cov", y.field = NULL, rel2abs = T) {
  if (is.null(y.field)) {
    if (identical(field, "decomposed_cov"))
      y.field = "foreground"
    else if (identical(field, "cbs_cov_rds"))
      y.field = "ratio"
  }
  if (is.null(id) && nrow(pairs) == 1) {
    covpath = pairs[[field]]
    if (isTRUE(rel2abs))
      jabpath = pairs$jabba_rds
  } else if (!is.null(id) && length(id) == 1) {
    covpath = pairs[id][[field]]
    if (isTRUE(rel2abs))
      jabpath = pairs$jabba_rds
  }
  cov = readRDS(covpath)
  if (isTRUE(rel2abs)) {
    lst.pp = with(readRDS(jabpath), list(purity = purity, ploidy = ploidy))
    mcols(cov)[[paste0(y.field, "_old")]] = mcols(cov)[[y.field]]
    mcols(cov)[[y.field]] = skitools::rel2abs(cov,
                                   field = y.field,
                                   purity = lst.pp$purity,
                                   ploidy = lst.pp$ploidy)
  }
  coln = colnames(mcols(cov))
  y.id = match3(y.field, coln)
  nony.id = seq_along(coln)[-y.id]
  return(et(sprintf("cov[,%s,drop=FALSE]", mkst(c(y.id, nony.id)))))
}

#' @name grabggtrack
#' @title convenience function to pull out jabba model + coverage
#'
#'
#' @return
#' @export
grabggtrack = function(pairs, pair = NULL, jab_field = "complex", cov_field = "decomposed_cov", cov_y_field = NULL) {
  if (!is.null(pair)) {
    if (length(pair) > 1) stop("provide only one id or index")
    pairs = pairs[pair, nomatch = 0]
  }
  if (nrow(pairs) != 1) stop("something messed up")
  this.env = environment()
  gg = gG(jabba = pairs[[jab_field]])
  gg = copy2(gg)
  gg$edges$mark(col = NULL)
  gg$nodes$mark(col = NULL)
  cov = grabcov(pairs, field = cov_field, y.field = cov_y_field)
  gt.cov = gTrack(cov, colnames(mcols(cov))[1], circles = T, lwd.border = 0.001)
  gt = c(gt.cov, gg$gtrack())
  return(gt)
}



## setMethod("with", signature(data = "gTrack"), NULL)
## setMethod("with", signature(data = "gTrack"), function(data, expr) {
##     df = as.data.frame(formatting(data))
##     eval(substitute(expr, parent.frame()), df, parent.frame())
## })

## setMethod("within", signature(data = "gTrack"), NULL)
## setMethod("within", signature(data = "gTrack"), function(data, expr) {
##     e = list2env(as.list(formatting(data)))
##     eval(substitute(expr, parent.frame()), e, parent.frame())
##     formatting(data) = as.data.frame(as.list(e))[, c(colnames(formatting(data))),drop = FALSE]
##     return(data)
## })

#' @name grabhjab
#' @title convenience function to pull out jabba model with allelic cn
#'
#' Convenience function uses a pairs table entry
#' and hunts for jabba. If allelic cn fields aren't present,
#' will look for hetpileups data and run JaBbA:::jabba.alleles
#'
#' @return
#' @export
grabhjab = function(pairs, id = NULL) {

    if (!is.null(id)) {
        pairs = pairs[id]
    } else if (len(pairs) != 1) {
        stop("pairs must be length 1")
    }

    jab = readRDS(pairs$jabba_rds)
    if (is.null(jab$agtrack)) {
        het.sites = with(pairs, coalesce(het_pileups_wgs, maf_approx, hmf_germline_txt, bads = Negate(file.exists2)))
        if (grepl(".rds$", het.sites)) {
            het.sites = readRDS(het.sites) %>% within({alt = alt.count.t; ref = ref.count.t})
        } else if (grepl(".txt(.gz){0,}", het.sites)) {
            het.sites = df2gr(fread(het.sites), 1, 2, 3)
            df = mcols(het.sites)
            mcols(het.sites) = setcols(df, c("REF_AD", "ALT_AD"), c("ref", "alt"))
        } else if (!file.exists2(het.sites)) {
            warning("no hets")
        }
        jaba = JaBbA:::jabba.alleles(jab, het.sites, uncoupled = TRUE)
        ## agt = jaba$agtrack
    } else {
        jaba = jab
    }
    
    if (is.null(jaba$agtrack)) stop("no allelic cn available!")

    return(jaba)
}


#' @name gt.fix
#' @title fix gtrack metadata elements for plotting
#'
#' @description
#' Parse a tabular summary of a glm model
#'
#' @return A data.frame/data.table
#' @export gt.fix
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

#' @name extract_ggplot
#' @title helper function to grab relevant ggplot_build outputs
#'
#' @description
#' takes an input function and grabs the processed data frame
#' that ggplot employs to draw its plots
#'
#' Note: reordering plot data to fit the ggplot wrangled data is
#' (if there is a 1 to 1 mapping)
#' plot_data[order(<facet.x>, <facet.y>, <grouping_variable>)]
#'
#' OR
#'
#' plot_data[order(PANEL, <grouping_variable>)]
#'
#' useful for custom construction of layers
#'
#' @param ggplot_obj A ggplot object
#' @export
extract_ggplot = function(ggplot_obj) {
    gb = copy3(ggplot_build(ggplot_obj))
    gg.x = all.vars(gb$plot$mapping$x)
    if (NROW(gg.x)) gg.x = rlang::quo_name(gb$plot$mapping$x)
    gg.y = all.vars(gb$plot$mapping$y)
    if (NROW(gg.y)) gg.y = rlang::quo_name(gb$plot$mapping$y)
    gb.layout = as.data.table(gb$layout$layout)
    lst.d = lapply(gb$data, function(layer) {
        if (!inherits(layer$PANEL, "factor")) layer$PANEL = factor(layer$PANEL)
        return(merge.data.table(as.data.table(layer), gb.layout, by = c("PANEL")))
    })
    facet_nm = lapply(gb$plot$facet$params$facets, quo_name)
    facet.x = facet_nm[1][[1]]
    facet.y = facet_nm[2][[1]]
    plot_layers = gb$plot$layers
    lst.players = lapply(plot_layers, function(plot_layer) {
        x.field = all.vars(plot_layer$mapping$x)
        if (NROW(x.field)) x.field = rlang::quo_name(plot_layer$mapping$x)
        y.field = all.vars(plot_layer$mapping$y)
        if (NROW(y.field)) y.field = rlang::quo_name(plot_layer$mapping$y)
        list(x.field = x.field, y.field = y.field)
    })

    lst.gplayers = lapply(plot_layers, function(plot_layer) {

        x.field = all.vars(plot_layer$mapping$x)
        if (NROW(x.field)) x.field = rlang::quo_name(plot_layer$mapping$x)
        y.field = all.vars(plot_layer$mapping$y)
        if (NROW(y.field)) y.field = rlang::quo_name(plot_layer$mapping$y)
        list(x.field = x.field, y.field = y.field)
    })
    gb.plot.data = gb$plot$data
    gb.layers = gb$plot$layers
    if (class(gb.plot.data)[1] == "waiver") gb.plot.data = structure(gb.plot.data, class = NULL)
    gb.plot.data = as.data.table(gb.plot.data)
    lst.plotdata = mapply(function(data_, exprlst)
    {
        if (NROW(data_)) {
            if (NROW(exprlst$x.field))
                data_[["x"]] = eval(parse(text = exprlst$x.field), data_)
            if (NROW(exprlst$y.field))
                data_[["y"]] = eval(parse(text = exprlst$y.field), data_)
            data_ = tryCatch(merge.repl(data_, gb.layout), error = function(e) data_)
            ## data_ = merge.repl(data_, gb.layout)
            ## data_[["facet.x"]] = if (NROW(facet.x)) eval(parse(text = facet.x), data_) else ""
            ## data_[["facet.y"]] = if (NROW(facet.y)) eval(parse(text = facet.y), data_) else ""
            ## if (all(colexists(c("facet.x", "facet.y"), data_)))
            ##     data_[order(facet.x, facet.y), PANEL := .GRP, by = .(facet.x, facet.y)]
        }
        return(data_)
    }, list(gb.plot.data), lst.players, SIMPLIFY = FALSE)
    lst.layerdata = mapply(function(data_, exprlst)
    {
        data_ = data_$data
        if (class(data_)[1] == "waiver") data_ = structure(data_, class = NULL)
        if (NROW(data_)) {
            data_ = as.data.table(data_)
            if (NROW(exprlst$x.field))
                data_[["x"]] = eval(parse(text = exprlst$x.field), data_)
            if (NROW(exprlst$y.field))
                data_[["y"]] = eval(parse(text = exprlst$y.field), data_)
            data_ = tryCatch(merge.repl(data_, gb.layout), error = function(e) data_)
            ## data_ = merge.repl(data_, gb.layout)
            ## data_[["facet.x"]] = if (NROW(facet.x)) eval(parse(text = facet.x), data_) else ""
            ## data_[["facet.y"]] = if (NROW(facet.y)) eval(parse(text = facet.y), data_) else ""
            ## if (all(colexists(c("facet.x", "facet.y"), data_)))
            ##     data_[order(facet.x, facet.y), PANEL := .GRP, by = .(facet.x, facet.y)]
        }
        return(data_)
    }, gb.layers, lst.players, SIMPLIFY = FALSE)
    list(lst.d = lst.d, lst.plotdata = lst.plotdata, lst.players = lst.players, lst.layerdata = lst.layerdata, facet.x = facet.x, facet.y = facet.y, gg.x = gg.x, gg.y = gg.y, layout = gb.layout)
}


#' @name integer_breaks
#' @title make integer breaks on axes
#'
#' a la scales::breaks_pretty
#'
#' @export
integer_breaks <- function(n = 5, ...) {
    fxn <- function(x) {
        breaks <- floor(pretty(x, n, ...))
        names(breaks) <- attr(breaks, "labels")
        breaks
    }
    return(fxn)
}


#' @name gg_mytheme
#' @title custom theme for ggplot
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
                      x_axis_hjust = 1,
                      x_axis_vjust = 0.5,
                      y_axis_hjust = 1,
                      print = FALSE) {
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
    if (isTRUE(print)) return(print(gg)) else return(gg)
}

#' @name pg_mytheme
#' @title print gg_mytheme output
#'
#' convenience wrapper around gg_mytheme
#' to automatically print the output
#'
#' @return A ggplot object
#' @export
pg_mytheme = function(..., print = TRUE) gg_mytheme(..., print = TRUE)


#' @name gg.sline
#' @title wrapper around geom_point and geom_smooth
#'
#' a wrapper around geom_smooth to fit dot plot with lm line
#'
#' @param x numeric values
#' @param y numeric values
#' @param group vector parallel to x and y that specifies grouping
#' @param colour vector that specifies colouring of points
#' @return A ggplot object
#' @export gg.sline
gg.sline = function(x, y, group = "x", colour = NULL, smethod = "lm", dens_type = c("point", "hex"), facet1 = NULL, facet2 = NULL, transpose = FALSE, facet_scales = "fixed", line = TRUE, formula = y ~ x, print = FALSE, hex_par = list(bins = 50), wes = NULL, cex.scatter = 0.1) {
    if (is.null(facet1)) {
        facet1 = facet2
        facet2 = NULL
    }
    if (!is.null(facet1))
        if (!is.factor(facet1))
            facet1 = factor(facet1, unique(facet1))
    if (!is.null(facet2))
        if (!is.factor(facet2))
            facet2 = factor(facet2, unique(facet2))
    dat = data.table(x, y, group, facet1 = facet1, facet2 = facet2)
    gg = ggplot(dat, mapping = aes(x = x, y = y, group = group))
    if (identical(dens_type, c("point", "hex"))) {
        message("selecting geom_point() as default")
        dens_type = "point"
    }
    if (isTRUE(line)) {
        gg = gg +
            geom_smooth(method = smethod, size = 1, formula = formula)
    }
    if (identical(dens_type, "hex"))
        gg = gg + geom_hex(bins = hex_par$bin)
    else if (identical(dens_type, "point"))
        if (is.null(colour))
            gg = gg + geom_point(size = cex.scatter)
        else
            gg = gg + geom_point(mapping = aes(colour = colour), size = cex.scatter)
    if (!is.null(dat$facet1)) {
        if (!is.null(dat$facet2)) {
            if (transpose)
                gg = gg + facet_grid(facet2 ~ facet1, scales = facet_scales)
            else gg = gg + facet_grid(facet1 ~ facet2, scales = facet_scales)
        }
        else {
            if (transpose)
                gg = gg + facet_grid(. ~ facet1, scales = facet_scales)
            else gg = gg + facet_grid(facet1 ~ ., scales = facet_scales)
        }
    }
    if (!is.null(wes) && is.character(wes))
        gg = gg + scale_colour_manual(values = brewer.master(length(unique(colour)), wes = TRUE, palette = wes))
    if (print)
        print(gg)
    else
        gg
}


#' @name gbar.error
#' @title barplot with errorbars
#'
#' Barplot with confidence intervals.
#' To plot with confidence intervals around an event that has a binary
#' outcome with a simple example:
#'
#' heads = 30, tails = 20,
#' y = heads / (heads + tails)
#' dt = binom.conf(y, heads + tails)
#' conf.low = dt$conf.low
#' conf.high = dt$conf.high
#'
#' gbar.error(y, conf.low, conf.high)
#'
#' @param y any numeric vector, can be a fraction
#' @param conf.low the lower bound of the confidence interval around y
#' @param conf.high the upper bound of the confidence interval around y
#'
#' @return A ggplot object
#' @export gbar.error
gbar.error = function(y, conf.low, conf.high, group, wes = "Royal1", other.palette = NULL, print = TRUE, fill = NULL, stat = "identity", facet1 = NULL, facet2 = NULL, bar.width = 0.9, position = position_dodge(width = 0.9), transpose = FALSE, facet.scales = "fixed") {
    dat = data.table(y = y, conf.low = conf.low, conf.high = conf.high, group = group)
    if (is.null(facet1)) {
        facet1 = facet2
        facet2 = NULL
    }
    if (!is.null(facet1))
        if (!is.factor(facet1))
            facet1 = factor(facet1, unique(facet1))
    if (!is.null(facet2))
        if (!is.factor(facet2))
            facet2 = factor(facet2, unique(facet2))
    suppressWarnings(dat[, `:=`(facet1, facet1)])
    suppressWarnings(dat[, `:=`(facet2, facet2)])
    if (is.null(fill)) fill.arg = group else fill.arg = fill
    dat[, fill.arg := fill.arg]
    if (is.character(stat) && stat == "count")
        gg = ggplot(dat, aes(fill = fill.arg, x = y))
    else
        gg = ggplot(dat, aes(x = group, fill = fill.arg, y = y))
    gg = gg + geom_bar(stat = stat, position = position, width = bar.width)
    if (any(!is.na(conf.low)) & any(!is.na(conf.high)))
        gg = gg + geom_errorbar(aes(ymin = conf.low, ymax = conf.high), size = 0.1, width = 0.3, position = position)
    if (!is.null(wes))
        gg = gg + scale_fill_manual(values = skitools::brewer.master(n = length(unique(fill.arg)), wes = TRUE, palette = wes))
        ## gg = gg + scale_fill_manual(values = wesanderson::wes_palette(wes))
    if (!is.null(other.palette))
        gg = gg + scale_fill_manual(values = other.palette)
    if (!is.null(dat$facet1)) {
        if (!is.null(dat$facet2)) {
            if (transpose)
                gg = gg + facet_grid(facet2 ~ facet1, scales = facet.scales)
            else gg = gg + facet_grid(facet1 ~ facet2, scales = facet.scales)
        }
        else {
            if (transpose)
                gg = gg + facet_grid(. ~ facet1, scales = facet.scales)
            else gg = gg + facet_grid(facet1 ~ ., scales = facet.scales)
        }
    }
    if (print) print(gg) else gg
}



#' @name gg.hist
#' @title generate ggplot histogram
#'
#' generate ggplot histogram
#'
#' @return A ggplot object
#' @export gg.hist
gg.hist = function(dat.x, as.frac = FALSE, bins = 50, center = NULL, boundary = NULL, trans = "identity", print = TRUE, xlim = NULL, ylim = NULL, xlab = "", x_breaks = 20, y_breaks = 10, expand = waiver(), ...) {
    gg = ggplot(mapping = aes(x = dat.x))
    if (isTRUE(as.frac))
        gg = gg + geom_histogram(aes(y = ..count.. / sum(..count..)), bins = bins, ...)
    else
        ## gg = gg + geom_histogram(stat = stat_bin(bins = bins), ...)
        gg = gg + geom_histogram(bins = bins, ...)
    gg = gg + scale_x_continuous(trans = trans, limits = xlim, breaks = scales::pretty_breaks(n = x_breaks)) +
        scale_y_continuous(breaks = scales::pretty_breaks(n = y_breaks), limits = ylim, expand = expand)
    if (!is.null(xlab) && any(!is.na(xlab)) && nzchar(xlab))
        gg = gg + xlab(xlab)
    if (print)
        print(gg)
    else
        gg
}



##############################
############################## htslib / skidb stuff
##############################

#' @name vcf_remove_sample
#' @title take vcf read in chunks and write clean vcf
#'
#' take vcf read in chunks and write clean vcf without sample info
#' i.e. fixing up svaba indel crap
#'
#' @return character
#' @export vcf_remove_sample
vcf_remove_sample = function(x = '/gpfs/commons/groups/imielinski_lab/data/PCAWG/mutations/f393baf9-2710-9203-e040-11ac0d484504,vcf',
                             out.vcf = "~/outtest.vcf",
                             ref = "~/DB/GATK/hg19_gatk_decoy.fasta", chunk = 10000,
                             verbose = TRUE) {
    ## fa = readinfasta(ref)
    f = file(x, "r")
    r = readLines(f, n = 1)
    ro = grep("^#", r, invert = F, value = T)
    header = c()
    while (NROW(ro)) {
        header = c(header, ro)
        r = readLines(f, n = 1)
        ro = grep("^#", r, invert = F, value = T)
    }
    contents = c()
    system2('rm', out.vcf)
    system2('touch', out.vcf)
    header.l = paste0(strsplit(header[length(header)], "\t")[[1]][-c(10:10000)], collapse = "\t")
    c(header[-length(header)], header.l)
    writeLines(c(header[-length(header)], header.l), out.vcf)
    while (NROW(r)) {
        tb = read.table(text = r, fill = T, sep = "\t", colClasses = "character")
        ## tb = fread(text = r, fill = T, sep = "\t", colClasses = "character")
        tb = tb[,-c(10:NCOL2(tb)),]
        ## tb$V9 = "."
        ## ins = which(tb$V4 == ".") ## Insertion, grab REF base at coordinate, add REF nucleotides to left of ALT, REF should be 1 base
        ## del = which(tb$V5 == ".") ## Deletion, subtract coordinate by 1, grab REF base at coordinate, add REF nucleotide to left of REF, ALT should be 1 base which is the REF
        ## if (NROW(del) > 0) {
        ##     tb[del,]$V2 = tb[del,]$V2 - 1
        ##     gr = with(tb[del,], GRanges(V1, IRanges(V2, V2)))
        ##     REF = as.character(fa[gr])
        ##     tb[del,]$V5 = REF
        ##     tb[del,]$V4 = paste0(REF, tb[del,]$V4)
        ## }
        ## if (NROW(ins) > 0) {
        ##     gr = with(tb[ins,], GRanges(V1, IRanges(V2, V2)))
        ##     REF = as.character(fa[gr])
        ##     tb[ins,]$V5 = paste0(REF, tb[ins,]$V5)
        ##     tb[ins,]$V4 = REF
        ## }
        tb$V9 = "."
        ## tb$V10 = "."
        fwrite(tb, out.vcf, append = T, sep = "\t")
        if (length(r) > 0 && verbose) message('processed: ', length(r), " variants")
        r = readLines(f, n = chunk)
    }
    out.vcf

    ## out.vcf = paste0(tools::file_path_sans_ext(path), "_fixed.vcf")
    ## sl = enframe(hg_seqlengths())
    ## cl = paste0("##contig=<ID=", sl[[1]], ",length=",sl[[2]], ">")
    ## system2("rm", c("-f", out.vcf)); system2("touch", out.vcf)
    ## writeLines(c("##fileformat=VCFv4.2", cl, "#CHROM\tPOS\tID\tREF\tALT\tQUAL\tFILTER\tINFO\tFORMAT\tsample"), out.vcf)
    ## fwrite(out, out.vcf, append = T, sep = "\t", col.names = FALSE)
    ## data.table(pair = p, mut_consensus_vcf = out.vcf)

}


#' @name write_bed
#' @title write bed or bedpe into canonical formatted table
#'
#' Write a table into  a bed oe bedpe formatted file
#' Ensures the header is commented out
#'
#' @return A data.table
#' @export write_bed
write_bed = function(bed, outpath) {
    cn = colnames(bed)
    cn[1] = paste0("#", cn[1])
    bedhead = paste0(cn, collapse = "\t")
    writeLines(bedhead, outpath)
    tryCatch(fwrite(bed, outpath, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE),
             error = function(e) {
                 write.table(bed, outpath, sep = "\t", col.names = FALSE, row.names = FALSE, append = TRUE, quote = FALSE)
             })
}

#' @name read_bed
#' @title read bed or bedpe as a table
#'
#' Read in a bed oe bedpe formatted file into tabular format
#'
#' @return A data.table
#' @export read_bed
read_bed = function(bedpath) {
    f = file(bedpath, open = "r")
    thisline = readLines(f, 1)
    headers = character(0)
    while (length(grep("^((#)|(chrom)|(chr))", thisline, ignore.case = T))) {
        headers = c(headers, thisline)
        thisline = readLines(f, 1)
    }    
    lastheader = tail(headers, 1)
    ## ln = sum(length(headers), length(thisline))
    ## while (length(thisline) > 0) {
    ##     ## thisline = readBin(f, "raw", n = 50000)
    ##     ## sum(thisline == as.raw(10L))
    ##     thisline = readLines(f, n = 50000)
    ##     ln = length(thisline) + ln
    ## }
    ## fread(bedpath, skip = length(headers))
    ## bed = tryCatch(fread(bedpath, skip = NROW(headers), header = F),
    ##                error = function(e) {
    ##                    read.table(bedpath, comment.char = "", skip = NROW(headers), header = F)
    ##                })
    bedhead = gsub("^#", "", unlist(strsplit(lastheader, "\t")))
    bed = tryCatch(fread(bedpath, skip = NROW(headers), header = F), 
                   error = function(e) NULL)
    if (is.null(bed)) 
        bed = tryCatch(read.table(bedpath, comment.char = "", skip = NROW(headers), 
                                  header = F), error = function(e) NULL)
    
    if (is.null(bed)) {
        bed = matrix(integer(0), ncol = length(bedhead))
        bed = as.data.table(bed)
    }

    if (identical(NROW(bedhead), ncol(bed))) {
        colnames(bed) = bedhead
    }
    return(bed)
}

#' @name read.bam.header
#' @title read.bam.header
#'
#' Read in a bam header into tabular format
#'
#' @return A data.table
#' @export read.bam.header
read.bam.header = function(bam, trim = FALSE) {
    cmd = sprintf("samtools view -H %s", bam)
    ## tb = fread(text = system(cmd, intern = TRUE), fill = TRUE, sep = "\t", header = F)
    tb = setDT(read.table(text = system(cmd, intern = TRUE), fill = TRUE, sep = "\t", header = F))
    if (!trim) {
        return(as.data.table(tb))
    } else {
        tb = tb[grepl("^SN", V2)][, V2 := gsub("SN:", "", V2)]
        identity(tb)
        return(withAutoprint(tb, echo = FALSE)$value)
    }
}




#' @name bcfindex
#' @title bcfindex
#'
#' index a bcf/vcf file
#'
#' @return vcf path
#' @export
bcfindex = function(vcf, force = TRUE) {
    ## if (!force) {
    if (!grepl(".[bv]cf(.gz)?$", vcf)) {
        stop("check if you have a valid bcf/vcf file")
    }
    if (!file.exists(paste0(vcf, ".tbi")) & !file.exists(paste0(vcf, ".csi")) || isTRUE(force)) {
        system(sprintf("bcftools index --tbi %s", vcf))
    }
    vcf
}



#' @name read_vcf2
#' @title read_vcf2
#'
#' read in a vcf file to granges with additional processing with bcftools
#'
#' @return GRanges
#' @export
read_vcf2 = function(fn, gr = NULL, type = c("snps", "indels", "all"), hg = 'hg19', geno = NULL, swap.header = NULL, verbose = FALSE, add.path = FALSE, tmp.dir = tempdir(), ...) {
    if (any(!type %in% c("snps", "indels", "all"))) {
        stop("type must be one of \"snps\", \"indels\", \"all\"")
    }
    if (!missing(type)) {
        if ("all" %in% type) {
            v_query = ""
            message("grabbing all variants")
        } else if (all(c("snps", "indels") %in% type)) {
            v_query = "-v snps,indels"
            message("grabbing snps and indels")
        } else {
            v_query = sprintf("-v %s", type)
            message(sprintf("grabbing %s", type))
        }
    } else {
        v_query = ""
        message("Assuming all variants should be grabbed")
    }
    tmp.vcf = tempfile("tmp", fileext = ".vcf")
    if (!is.null(gr))
        grs = paste(gr.string(gr.stripstrand(gr)), collapse = " ")
    else
        grs = ""
    cmd = sprintf("(bcftools view %s -i 'FILTER==\"PASS\"' %s %s | bcftools norm -Ov -m-any) > %s", v_query, fn, grs, tmp.vcf)
    system(cmd)
    vars = gr.nochr(unname(read_vcf(tmp.vcf, gr = gr, hg = hg, geno = geno, swap.header = swap.header, verbose = verbose, add.path = add.path, tmp.dir = tmp.dir, ...)))
    vars$REF = as.character(vars$REF)
    vars$ALT = as.character(unstrsplit(vars$ALT))
    vars$type = with(mcols(vars), ifelse(nchar(REF) < nchar(ALT), "INS",
                                  ifelse(nchar(REF) > nchar(ALT), "DEL",
                                  ifelse(nchar(REF) == 1 & nchar(ALT) == 1, "SNV", NA_character_))))
    vars = sort(sortSeqlevels(vars), ignore.strand = FALSE)
    return(vars)
}




##############################
##############################
############################## data.table and general data.frame utilities
##############################
##############################



#' @name trans.df
#' @title transpose data.table or data.frame
#'
#' @description
#' transpose a data frame
#' by default, the first column is stripped
#' and used as colnames of the output table
#'
#' @export trans.df
trans.df = function(df, rn = 1) {
    if (inherits(df, "data.table")) {
        df = asdf(df)
    }
    if (!(is.null(rn) || is.na(rn))) {
        rn.cols = dodo.call2(paste, .gc(df, rn))
        df = .gc(df, rn, invert = T)
    }
    out = data.table::transpose(df)
    if (!(is.null(rn) || is.na(rn))) {
        (data.table::setnames(out, rn.cols))
    }
    return(out)
}


#' @name dunlist2
#' @title unlist into a data.table
#'
#' unlisting a list into a data table with
#' ids corresponding to each list element
#'
#' @export dunlist2
dunlist2 = function (x, simple = FALSE)
{
    if (is.null(names(x)))
        names(x) = 1:length(x)
    tmp = x
    ## for (i in seq_along(tmp))
    ##     tmp[[i]] = as.data.table(tmp[[i]])
    if (!simple) {
        tmp = lapply(x, as.data.table)
        out = cbind(data.table(listid = rep(names(x), lens(x))),
                               rbindlist(tmp, fill = TRUE))
    } else {
        out = cbind(data.table(listid = rep(names(x), lens(x))),
                               V1 = unlist(tmp))
    }
    nm = unlist(lapply(x, names2))
    out$names = nm
    setkey(out, listid)
    return(out)
}


#' @name debug.s4
#' @title trace into an S4 function
#'
#' Debugging an S4 function can't be done with debug().
#' This function is a convenience wrapper around trace()
#' to step into S4 methods
#'
#' @export debug.s4
debug.s4 = function(what, signature, where) {
    if (is.character(where)) {
        where = asNamespace(sub("package:", "", where))
    }
    if (is.character(what)) {
        what = get0(what, mode = "function", envir = where)
    }
    trace(what = what, tracer = browser,
          at = 1,
          signature = signature, where = where)
}

#' @name undebug.s4
#' @title reverse debug.s4
#'
#' undebugging an S4 function when debug.s4/trace were
#' called on an S4 method.
#'
#' @export undebug.s4
undebug.s4 = function(what, signature, where) {
    if (is.character(where)) {
        where = asNamespace(sub("package:", "", where))
    }
    if (is.character(what)) {
        what = get0(what, mode = "function", envir = where)
    }
    untrace(what = what, signature = signature, where = where)
}

#' @name mstrsplit
#' @title make matrix out of stringsplitted character vector
#'
#' split a vector by a delimiter,
#' fill in to make same length
#' bind into matrix
#' not as efficient as data.table::tstrsplit
#' but this is entirely base R
#'
#' @return a matrix
#' @export
mstrsplit = function(x, ...) {
    lst = strsplit(x = x, ...)
    mlen = max(lengths(lst))
    unid = seq_len(mlen)
    for (i in seq_along(lst)) {
        length(lst[[i]]) = mlen
    }
    return(do.call(rbind, lst))
}



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
#' @title merging data tables with collapsing columns with the same name
#'
#' Merge two data tables with various replacing strategies
#' for columns common between x and y that are not used to merge
#' (i.e. not specified in the "by" argument)
#'
#' @param replace_NA logical, only use values in dt.y, any dt.x not in dt.y is clobbered (NA)
#' @param force_y logical, should x and y common columns be merged?
#' @param overwrite_x logical, if force_y = TRUE, should NA values in y replace x?
#' @return A data.table
#' @export merge.repl
merge.repl = function(dt.x,
                      dt.y,
                      sep = "_",
                      replace_NA = TRUE,
                      force_y = TRUE,
                      overwrite_x = FALSE,
                      keep_order = FALSE,
                      keep_colorder = TRUE,
                      keep_factor = TRUE,
                      ...) {
    arg_lst = as.list(match.call())
    by.y = eval(arg_lst[['by.y']], parent.frame())
    by.x = eval(arg_lst[['by.x']], parent.frame())
    by = eval(arg_lst[['by']], parent.frame())
    all.x = eval(arg_lst[['all.x']], parent.frame())
    all.y = eval(arg_lst[['all.y']], parent.frame())
    all = eval(arg_lst[['all']], parent.frame())
    allow.cartesian = eval(arg_lst[['allow.cartesian']])
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
    if (keep_order == TRUE) {
        dt.x[['tmp.2345098712340987']] = seq_len(nrow(dt.x))
    }

    dt.x[['in.x.2345098712340987']] = rep(TRUE, length.out = nrow(dt.x))
    dt.y[['in.y.2345098712340987']] = rep(TRUE, length.out = nrow(dt.y))

    new_ddd_args = list(by = by, by.x = by.x, by.y = by.y, all.x = all.x, all.y = all.y, allow.cartesian = allow.cartesian)

    if (is.null(by.y) & is.null(by.x) & is.null(by)) {

        if (length(attributes(dt.x)[['sorted']]) > 0 &&
            length(attributes(dt.y)[['sorted']]) > 0) {
            k.x = key(dt.x)
            k.y = key(dt.y)
        } else {
            k.y = k.x = intersect(names2(dt.x), names2(dt.y))
            if (length(k.x) == 0)
                stop("no common columns to merge by!")
            message("intersecting by: ", paste(k.x, collapse = ", "))
            new_ddd_args[['by']] = k.x
        }
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
        if (! all(by %in% colnames(dt.x)) | ! all(by %in% colnames(dt.y))) {
            stop("column ", by, " does not exist in one of the tables supplied \nCheck the column names")
        }
        new_ddd_args = new_ddd_args[setdiff(names(new_ddd_args), c("by.y", "by.x"))]

    }
    these_cols = intersect(x.cols, y.cols)
    ## if (replace_in_x) {
    if (!replace_NA) {
        dt.x.tmp = copy(dt.x)
        for (this_col in these_cols) {
            data.table::set(dt.x.tmp, i = NULL, j = this_col, value = NULL)
        }
        dt.repl = suppressWarnings(do.call("merge", args = c(list(x = dt.x.tmp, y = dt.y), new_ddd_args)))
        ## dt_na2false(dt.repl, c("in.x.2345098712340987", "in.y.2345098712340987"))
    } else {
        dt.repl = suppressWarnings(do.call("merge", args = c(list(x = dt.x, y = dt.y), new_ddd_args)))
        dt_na2false(dt.repl, c("in.x.2345098712340987", "in.y.2345098712340987"))
        in.x = which(dt.repl[["in.x.2345098712340987"]])
        in.y = which(dt.repl[["in.y.2345098712340987"]])
        this_env = environment()
        for (this_col in these_cols) {
            x_cname = paste0(this_col, ".x")
            y_cname = paste0(this_col, ".y")
            x_col = dt.repl[[x_cname]]
            y_col = dt.repl[[y_cname]]
            xf = inherits(x_col, "factor")
            yf = inherits(y_col, "factor")
            if ( {xf || yf} && keep_factor) {
                if (!xf) { x_col = factor(x_col); xf = TRUE }
                if (!yf) { y_col = factor(y_col); yf = TRUE }
            }
            if (xf && !keep_factor) { x_col = as.character(x_col); xf = FALSE } 
            if (yf && !keep_factor) { y_col = as.character(y_col); yf = FALSE }
            if (force_y) {
                if (!overwrite_x) {
                    ## if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                    ##     new_col = factor(y_col, forcats::lvls_union(list(y_col, x_col)))
                    ##     new_col[is.na(new_col)] = x_col[is.na(new_col)]
                    ## } else {
                    ##     new_col = ifelse(!is.na(y_col), y_col, x_col)
                    ## }                    
                    if (xf || yf) {
                        new_col = factor(y_col, forcats::lvls_union(list(y_col, x_col)))
                        new_col[is.na(new_col)] = x_col[is.na(new_col)]
                    } else {
                        new_col = ifelse(!is.na(y_col), y_col, x_col)
                    }
                } else {
                    ## if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                    ##     new_col = factor(x_col, forcats::lvls_union(list(y_col, x_col)))
                    ## } else {
                    ##     new_col = x_col
                    ## }
                    ## new_col[dt.repl[['in.y.2345098712340987']]] = y_col[dt.repl[['in.y.2345098712340987']]]
                    if (xf || yf) {
                        new_col = factor(x_col, forcats::lvls_union(list(y_col, x_col)))
                    } else {
                        new_col = x_col
                    }
                    new_col[in.y] = y_col[in.y]
                }
            } else {
                ## if (inherits(x_col, "factor") & inherits(y_col, "factor")) {
                ##     new_col = factor(x_col, forcats::lvls_union(list(x_col, y_col)))
                ##     new_col[is.na(new_col) & !is.na(y_col)] = y_col[is.na(new_col) & !is.na(y_col)]
                ## } else {
                ##     new_col = ifelse(is.na(x_col) & !is.na(y_col), y_col, x_col)
                ## }
                if (xf | yf) {
                    new_col = factor(x_col, forcats::lvls_union(list(x_col, y_col)))
                    new_col[is.na(new_col) & !is.na(y_col)] = y_col[is.na(new_col) & !is.na(y_col)]
                } else {
                    new_col = ifelse(is.na(x_col) & !is.na(y_col), y_col, x_col)
                }
            }
            data.table::set(dt.repl, j = c(x_cname, y_cname, this_col), value = list(NULL, NULL, this_env[["new_col"]]))
        }
    }
    ## } else if (!replace_in_x & !is.null(suffix)) {
    ##     y.suff.cols = paste0(y.cols, sep, suffix)
    ##     ## dt.y.tmp = copy(dt.y)[, eval(dc(y.suff.cols)) := eval(dl(y.cols))][, eval(dc(y.cols)) := NULL]
    ##     dt.y.tmp = copy(dt.y)
    ##     data.table::set(dt.y, j = y.suff.cols, value = dt.y[, y.cols, with = FALSE])
    ##     data.table::set(dt.y, j = y.cols, value = NULL)
    ##     ## dt.repl = merge(dt.x, dt.y.tmp, all.x = TRUE, ...)
    ##     dt.repl = do.call("merge", args = c(list(x = dt.x, y = dt.y.tmp), new_ddd_args))
    ## }
    if (keep_order == TRUE) {
        data.table::setorderv(dt.repl, "tmp.2345098712340987")
        dt.repl[['tmp.2345098712340987']] = NULL
    }
    data.table::set(dt.repl, j = c("in.y.2345098712340987", "in.x.2345098712340987"),
                    value = list(NULL, NULL))
    if (keep_colorder) {
        x_cols = colnames(dt.x)
        ## get the order of columns in dt.repl in order of X with
        ## additional columns tacked on end
        setcolorder(dt.repl,
                    intersect(union(colnames(dt.x), colnames(dt.repl)),
                              colnames(dt.repl)))
    }
    return(dt.repl)
}


#' @name dt_lg2int
#' @title logical to integer in data tables
#'
#' coerce logical columns to integers in data table
#'
#' @param dt data.table
#' @return A data.table
#' @export
dt_lg2int = function(dt) {
    these_cols = which(sapply(dt, class) == "logical")
    for (this_col in these_cols) {
        this_val = as.data.frame(dt[, this_col, with = FALSE])[,1]
        data.table::set(dt, j = this_col, value = as.integer(this_val))
    }
    return(dt)
}

#' @name dt_na2false
#' @title convert columns with NA to false
#'
#' coerce NA in columns of class "logical" to FALSE
#'
#' @param dt data.table
#' @param these_cols NULL by default, will select columns of class logical, otherwise will be specified
#' @return A data.table
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


#' @name dt_na2true
#' @title convert columns with NA to true
#'
#' coerce NA in columns of class "logical" to TRUE
#'
#' @param dt data.table
#' @param these_cols NULL by default, will select columns of class logical, otherwise will be specified
#' @return A data.table
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

#' @name dt_na2zero
#' @title convert columns with NA to zeros
#'
#' coerce NA in columns of class "numeric"/"integer" to FALSE
#'
#' @param dt data.table
#' @param these_cols NULL by default, will select columns of numeric class, otherwise will be specified
#' @return A data.table
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

#' @name dt_na2empty
#' @title convert columns with NA to empty character
#'
#' coerce NA in columns of class "character" to ""
#'
#' @param dt data.table
#' @param these_cols NULL by default, will select columns of character class, otherwise will be specified
#' @return A data.table
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


#' @name dt_empty2na
#' @title convert columns with empty character to NA
#'
#' coerce "" in columns of class "character" to NA_character_
#'
#' @param dt data.table
#' @param these_cols NULL by default, will select columns of character class, otherwise will be specified
#' @return A data.table
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
#' @name dt_empty2na
#' @title set columnn to NULL in data.table
#'
#' remove column from data table by setting to NULL
#'
#' @param dt data.table
#' @param cols character vector of column names
#' @return A data.table
#' @export
dt_setnull = function(dt, cols) {
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = NULL)
    }
    return(dt)
}

#' @name dt_setint
#' @title convert column to integer
#'
#' coerce column of type "numeric" by default to "integer"
#'
#' @param dt data.table
#' @param cols character vector of column names
#' @return A data.table
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

#' @name dt_setallna
#' @title convert all data.table entries to NA
#'
#' Set all columns to NA...
#' can't remember why one would want this...
#'
#' @param dt data.table
#' @param cols character vector of column names
#' @return A data.table
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

#' @name dt_setchar
#' @title convert column to character
#'
#' set columns to character
#' default is nonsensical?
#'
#' @param dt data.table
#' @param cols character vector of column names
#' @return A data.table
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

#' @name dt_any2lg
#' @title convert all columns to logical
#'
#' set columns to logical
#'
#' @param dt data.table
#' @param cols character vector of column names
#' @return A data.table
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

#' @name dt_f2char
#' @title convert factor columns to character
#'
#' coerce factor columns to character
#'
#' @param dt data.table
#' @param cols character vector of column names
#' @return A data.table
#' @export
dt_f2char = function(dt, cols = NULL) {
    if (is.null(cols)) {
        ## cols = names(dt)[which(!unlist(lapply(dt, class)) == "character")]
        cols = colnames(dt)[which(unlist(lapply(dt, class)) == "factor")]
    } else {
        cols = cols[which(unlist(lapply(as.list(dt)[cols], class)) == "factor")]
    }
    for (this_col in cols) {
        data.table::set(dt, j = this_col, value = as.character(dt[[this_col]]))
    }
    return(dt)
}


##############################
##############################
############################## Genomics / mskilab stuff
############################## gUtils stuff
##############################


make_granges <- function(x, seqnames.field = "seqnames", start.field = "start", end.field = "end", strand.field = "strand") {
    seqnames.ix = na.omit(match3(seqnames.field, colnames(x)))
    start.ix = na.omit(match3(start.field, colnames(x)))
    end.ix = na.omit(match3(end.field, colnames(x)))
    strand.ix = na.omit(match3(strand.field, colnames(x)))
    qseqnames.ix = na.omit(seqnames.ix[1])
    qstart.ix = na.omit(start.ix[1])
    qend.ix = na.omit(end.ix[1])
    qstrand.ix = na.omit(strand.ix[1])
    tmp = qmat(x,,c(qseqnames.ix, qstart.ix, qend.ix, qstrand.ix)) ## taking first match
    metas = qmat(x,,c(-seqnames.ix, -start.ix, -end.ix, -strand.ix))
    if (ncol(tmp) == 4)
        colnames(tmp) = c("seqnames", "start", "end", "strand")
    else
        colnames(tmp) = c("seqnames", "start", "end")
    gr = GRanges(tmp[["seqnames"]], IRanges(tmp[["start"]], tmp[["end"]]), strand = tmp[["strand"]])
    mcols(gr) = metas
    return(gr)
}

#' @name sim.bx
#' @title simulate 10x Barcoded reads off of collection of walks
#' 
#' @description
#' Take a collection of walks and simulate barcoded reads at the break sites
#' 
#'
#' @author Kevin Hadi
#' @export sim.bx
sim.bx <- function(wk, only_simulate_breaks = TRUE, numr = 24, bxwid = 30e3, physcov = 150, readlength = 150, fraglength = 1200) {
    ## NUMBX = numbx
    NUMR = numr
    
    if (inherits(wk, "gWalk")) {
        gw = wk
    } else if (inherits(wk, "GRangesList")) {
        gw = gW(grl = wk)
    }

    wk_seqnames = paste0(rep_len("W_", NROW(gw)), as.character(seq_along(gw)))

    ## spc = gChain::spChain(setNames2(gw$grl, wk_seqnames))
    
    grlby = gr_construct_by(grl.unlist(gw$grl), "grl.ix")
    grlby = split(grlby, mcols(grlby)$grl.ix)
    spc = gChain::spChain(setNames2(grlby, wk_seqnames))
    
    gw$set(alt.edge = gw$eval(edge = paste(which(type == "ALT"), collapse = ", ")))
    
    alt_ids = which(nzchar(gw$dt$alt.edge))
    any_alts = length(alt_ids) > 0

    get_ends = lapply(gGnome::lengths(gw), function(x) 1:(x - 1))

    ## alt_segs = grlby[IntegerList(strsplit(gw$dt$alt.edge, ", "))]
    ## breakends = gr.resize(alt_segs, width = 1, pad = FALSE, fix = "end")

    wkgr = spc@.galy
    wkgrl = split(wkgr, seqnames(wkgr))

    alt_segs = split(wkgr, seqnames(wkgr))[IntegerList(strsplit(gw$dt$alt.edge, ", "))]
    breakends = gr.resize(alt_segs, width = 1, pad = FALSE, fix = "end")
    breakpoints_somatic = unlist(gr.resize(breakends, width = 2, pad = FALSE, fix = "start"))


    ## if (any_alts) {
    ##     alt_ends = as.integer(unlist(strsplit(gw$dt$alt.edge, ", ")))
    ##     breakpoints_somatic = GRanges(wk_seqnames, IRanges(end(wkgr[alt_ends]), start(wkgr[alt_ends + 1])))
    ## }
    
    ## wkgr = dt2gr(gr2dt(wk[[1]])[, `:=`(end = cumsum(width))][, start := end - width + 1][, seqnames := "A"])

    ## totwid = sum(width(wk))
    totwids = width(wk)

    ## NUMBX = round(totwid * physcov / bxwid)
    NUMBX = round(totwids * physcov / bxwid)

    set_rngseed(10)

    ## bc.start = sample(max(end(wkgr)), NUMBX)

    bc.starts = mapply(function(wkend, numbx) {
        return(sample(wkend, numbx))
    }, max(end(wkgrl)), NUMBX, SIMPLIFY = F)

    gr.w = data.table(seqnames = rep(wk_seqnames, elementNROWS(bc.starts)),
                      start = unlist(bc.starts))
    gr.w[, end := (start + as.integer(jitter(rep(bxwid, length(start)), factor = 10))) - 1]
    gr.w[, BX := 1:.N]
    gr.w[, grl.ix := .GRP, by = seqnames]

    if (inherits(only_simulate_breaks, "logical")) {
        if (only_simulate_breaks) {
            ## interstitial_breaks = gr.end(wkgr[get_ends[[1]]])
            interstitial_breaks = gr.end(wkgrl[get_ends])
            query_footprint = GenomicRanges::reduce(interstitial_breaks + 1e5)
            query_footprint = unlist(query_footprint)
            
            ## gr.w = data.table(seqnames = "A", start = bc.start)
            ## gr.w[, end := (bc.start + as.integer(jitter(rep(bxwid, length(bc.start)), factor = 10))) - 1]
            ## gr.w[, BX := 1:.N]


            ## only simulate from those BX that are within query footprint
            ## let's not simulate the entire derivative chromosome
            bx.to.sim = (make_granges(gr.w) %&% query_footprint)$BX
            gr.w = gr.w[BX %in% bx.to.sim]

            bxwids = split(gr.w[, end + 1 - start], rleseq(gr.w$seqnames)$idx)

            ## NUMBX = length(bx.to.sim)
            NUMBX = gr.w[, length(unique(BX)), by = seqnames][, setNames2(V1, seqnames)]
        } else {
            bx.to.sim = 1:NROW(gr.w)
        }
    } else if (inherits(only_simulate_breaks, "GRanges")) {
        bx.to.sim = (make_granges(gr.w) %&% only_simulate_breaks)$BX
    }

    ## gr.w = GRanges("A", IRanges(bc.start, width = as.integer(jitter(rep(bxwid, length(bc.start)), factor = 10))))
    ## gr.w$BX = 1:NROW(gr.w)

    FRAGLENGTH=fraglength

    f.starts = mapply(function(bxwid, numbx) {
        sample(bxwid - FRAGLENGTH, numbx * NUMR, replace = T)
    }, bxwids, NUMBX, SIMPLIFY = F)
    
    ## f.starts = sample(bxwid - FRAGLENGTH, NUMBX * NUMR, replace = T)
    ## flst = split(f.starts, rep(1:NUMBX, each = NUMR))

    
    ## tmp = gr.sum(gr.w)
    ## sum(tmp$score * width(tmp)) / sum(width(tmp))



    ## bx.frag = GRanges("A", IRanges(rep(start(gr.w), each = NUMR) + f.starts, width = FRAGLENGTH))
    ## bx.frag$BX = rep(1:NUMBX, each = NUMR)
    ## bx.frag$qname = 1:NROW(bx.frag)

    READLENGTH=readlength

    ## bx.frag = data.table(seqnames = wk_seqnames, start = rep(gr.w$start, each = NUMR) + f.starts)
    ## bx.frag[, end := start + (FRAGLENGTH - 1)]
    ## bx.frag[, plus.end := start + (READLENGTH - 1)]
    ## bx.frag[, minus.start := end - (READLENGTH - 1)]
    ## bx.frag[, qname := 1:.N]
    ## bx.frag[, BX := rep(bx.to.sim, each= NUMR)]

    bx.frag = data.table(seqnames = rep(wk_seqnames, NUMBX * NUMR),
                         start = rep(gr.w$start, each = NUMR) + unlist(f.starts))
    bx.frag[, end := start + (FRAGLENGTH - 1)]
    bx.frag[, plus.end := start + (READLENGTH - 1)]
    bx.frag[, minus.start := end - (READLENGTH - 1)]
    bx.frag[, qname := 1:.N]
    bx.frag[, BX := rep(bx.to.sim, each= NUMR)]
    bx.frag[, grl.ix := .GRP, by = seqnames]
    

    plusreads = bx.frag[, .(seqnames, start, end = plus.end, strand= "+", BX, qname, grl.ix, readid = 1)]
    minusreads = bx.frag[, .(seqnames, start = minus.start, end = end, strand= "-", BX, qname, grl.ix, readid = 2)]

    ## r1 = gr.resize(bx.frag, 150, pad = FALSE, fix = "start")
    ## r2 = gr.resize(bx.frag, 150, pad = FALSE, fix = "end")

    ## grr = gr.spreduce(grbind(r1, r2), BX)
    grr = rbind(plusreads, minusreads)
    ## grr.range = gr_deconstruct_by(range(gr_construct_by(grr, "BX")), "BX")
    ## grr.frag = gr.sprange(grr, "BX")

    gr.grr = make_granges(grr)

    if (any_alts) {
        grr$split_read_support = gr.grr %^% (breakpoints_somatic)
        ## grr$split_read_support = gr2dt(grr)[, .(.I, split_supporting = any(split_read_support)), by = BX]$split_supporting
        bx.frag$discordant_read_support = make_granges(bx.frag) %^% breakpoints_somatic
        grr$discordant_read_support = grr$qname %in% bx.frag$qname[bx.frag$discordant_read_support]
        gr.w$bx_support = make_granges(gr.w) %^% breakpoints_somatic
        grr$bx_support = grr$BX %in% gr.w[gr.w$bx_support == TRUE]$BX
    } else {
        grr$split_read_support = FALSE
        grr$discordant_read_support = FALSE
        grr$bx_support = FALSE
    }

    mcols(gr.grr) = cbind(mcols(gr.grr), grr[,-c(1:8)])
    grr2 = gChain::lift(t(spc), gr.grr)

    grr2 = gr_deconstruct_by(grr2, "grl.ix")
    return(grr2)
}

#' @name bp2grl
#' @title convert table of paired string coordinates to GRangesList 
#' 
#' @description
#'
#'
#' @author Kevin Hadi
#' @export
bp2grl = function(df, bp1.field = "bp1", bp2.field = "bp2", sort = TRUE) {
    grl = grl.pivot(with(df, GRangesList(parse.gr(bp1), parse.gr(bp2))))
    mcols(grl) = df
    if (sort) {
        seqlevels(grl) = GenomeInfoDb::sortSeqlevels(seqlevels(grl))
        grl = sort.GRangesList(grl, ignore.strand = T)
    }
    return(grl)
}



#' @name findov
#' @title GenomicRanges::findOverlaps wrapper
#' 
#' @description
#'
#'
#' @author Kevin Hadi
#' @export
findov = function(query, subject, by = NULL, ...) {
    query = gr_construct_by(query, by = by)
    subject = gr_construct_by(subject, by = by)
    h = tryCatch(GenomicRanges::findOverlaps(query, subject, ...), error = function(e) {
        warning("findOverlaps applied to ranges with non-identical seqlengths")
        query = gUtils::gr.fix(query, subject)
        subject = gUtils::gr.fix(subject, query)
        return(GenomicRanges::findOverlaps(query, subject, ...))
    })
    tmp = setcols(asdt(h), c("query.id", "subject.id"))
    return(tmp)
}

#' @name gr.flipstrand
#' @title works with GRangesLists
#' 
#' @description
#'
#' to be used with gr_construct_by
#'
#' @return A GRanges with the by metadata field attached to the seqnames
#' @author Kevin Hadi
#' @export gr.flipstrand
gr.flipstrand = function(gr) {
    if (!inherits(gr, c("GRanges" ,"GRangesList"))) {
        stop("not a GRanges / GRangesList")
    }
    if (is(gr, "GRangesList")) {
        this.strand = gr@unlistData@strand
        gr@unlistData@strand = S4Vectors::Rle(factor(c("*" = "*", "+" = "-", "-" = "+")[as.character(this.strand)], levels(this.strand)))
    } else {
        this.strand = gr@strand
        gr@strand = S4Vectors::Rle(factor(c("*" = "*", "+" = "-", "-" = "+")[as.character(this.strand)], levels(this.strand)))
    }
    return(gr)
}


#' @name gr_deconstruct_by
#' @title removing by field and random string barcode to seqnames for more efficient by queries
#' 
#' @description
#'
#' to be used with gr_construct_by
#'
#' @return A GRanges with the by metadata field attached to the seqnames
#' @author Kevin Hadi
#' @export gr_deconstruct_by
gr_deconstruct_by <- function (x, by = NULL, meta = FALSE) 
{
    if (is.null(by) || length(x) == 0) 
        return(x)
    this.sep1 = " G89LbS7RCine "
    this.sep2 = " VxTofMAXRbkl "
    ans = x
    f1 = as.character(seqnames(x))
    f2 = sub(paste0(".*", this.sep2), "", f1)
    ## f2 = trimws(gsub(paste0(".*", this.sep2), "", f1))
    ## f2 = trimws(gsub(paste0(".*", this.sep1), "", f2))
    ui = which(!duplicated(f1))
    x_seqinfo <- seqinfo(x)
    seql = rleseq(f2[ui], clump = T)
    lst = lapply(split(seqlengths(x_seqinfo)[f1[ui]], seql$idx), 
        function(x) max(x))
    uii = which(!duplicated(f2[ui]))
    ans_seqlevels = f2[ui][uii]
    ans_seqlengths = setNames(unlist(lst), ans_seqlevels)
    ans_isCircular <- unname(isCircular(x_seqinfo))[ans_seqlevels]
    ans_seqinfo <- Seqinfo(ans_seqlevels, ans_seqlengths, ans_isCircular)
    ans@seqnames <- Rle(factor(f2, ans_seqlevels))
    ans@seqinfo <- ans_seqinfo
    if (isTRUE(meta)) {
        f0 = sub(paste0(this.sep2, ".*"), "", f1)
        f0 = data.table::tstrsplit(f0, this.sep1, fixed = T)
        ## f0 = strsplit(f0, this.sep1)
        ## f0 = trans(f0, c)
        mc = as.data.table(unname(f0))
        if (!(identical(by, "") | identical(by, NA) | identical(by, "NA")) &&
            length(by) == NCOL(mc)) {
            colnames(mc) = by
        }
        ## debugonce(do.assign)
        mcols(ans) = do.assign(mcols(ans), mc)
    }
    return(ans)
}


#' @name na.seql
#' @title NA out seqlevels
#' 
#' @description
#'
#' @return A GRanges with NA in all seqlevels
#' @author Kevin Hadi
#' @export na.seql
na.seql <- function (x) 
{
    x_seqinfo = seqinfo(x)
    ans = x
    ans_seqlengths = seqlengths(x_seqinfo)
    ans_seqlevels = seqlevels(x_seqinfo)
    ans_isCircular = isCircular(x_seqinfo)
    ans_seqlengths[] = NA_integer_
    ans_seqinfo = Seqinfo(ans_seqlevels, ans_seqlengths, ans_isCircular)
    ans@seqinfo = ans_seqinfo
    return(ans)
}



#' @name gr_construct_by
#' @title adding on by field to seqnames for more efficient by queries
#' 
#' @description
#'
#' Uses by field from metadata column to insert into seqnames
#' This is useful for more efficient queries findoverlaps queries between 2 ranges
#' when we want to stratify the query with a "by" field.
#' This feeds into the gr.findoverlaps family of gUtils tools.
#'
#' @return A GRanges with the by metadata field attached to the seqnames
#' @author Kevin Hadi
#' @export gr_construct_by
gr_construct_by = function(x, by = NULL, na.seql = TRUE) {
    if (is.null(by) || length(x) == 0) return(x)
    ## this.sep1 = {set.seed(10); paste0(" ", rand.string(), " ")}
    this.sep1 = " G89LbS7RCine "
    ## this.sep2 = {set.seed(11); paste0(" ", rand.string(), " ")}
    this.sep2 = " VxTofMAXRbkl "
    ## ans = copy2(x)
    ans = x
    thisp = function(...) paste(..., sep = this.sep1)
    f1 = do.call(paste, c(as.list(mcols(x)[, by, drop = FALSE]), sep = this.sep1))
    f2 = as.character(seqnames(x))
    f2i = as.integer(seqnames(x))
    f12 = paste(f1, f2, sep = this.sep2)
    ui = which(!duplicated(f12))
    ans_seqlevels = f12[ui]
    x_seqinfo <- seqinfo(x)
    ans_seqlengths = unname(seqlengths(x_seqinfo)[f2i[ui]])
    if (isTRUE(na.seql))
        ans_seqlengths[] = NA_integer_
    ans_isCircular <- unname(isCircular(x_seqinfo))[f2i[ui]]
    ans_seqinfo <- Seqinfo(ans_seqlevels, ans_seqlengths, ans_isCircular)
    ans@seqnames <- Rle(factor(f12, ans_seqlevels))
    ans@seqinfo <- ans_seqinfo
    return(ans)
}



#' @name shift_up
#' @title shift_upstream from plyranges package
#' @description
#'
#' works on genomicranges
#'
#' @return granges
#' @author Stuart Lee
#' @author Michael Lawrence
#' @author Dianne Cook
#' @export
shift_up = function (x, shift = 0L)
{
    strand = function(bla) {as.character(BiocGenerics::strand(bla))}
    neg <- strand(x) == "-"
    pos <- strand(x) %in% c("+", "*")
    if (length(x) == length(shift)) {
        shift_neg <- shift[which(neg)]
        shift_pos <- shift[which(pos)]
        x[neg] <- shift_right(x[neg], shift_neg)
        x[pos] <- shift_left(x[pos], shift_pos)
    }
    else {
        x[neg] <- shift_right(x[neg], shift)
        x[pos] <- shift_left(x[pos], shift)
    }
    return(x)
}


#' @name shift_down
#' @title shift_downstream from plyranges package
#' @description
#'
#' works on genomicranges
#'
#' @return granges
#' @author Stuart Lee
#' @author Michael Lawrence
#' @author Dianne Cook
#' @export
shift_down = function (x, shift = 0L)
{
    strand = function(bla) {as.character(BiocGenerics::strand(x))}
    neg <- strand(x) == "-"
    pos <- strand(x) %in% c("+", "*")
    if (length(x) == length(shift)) {
        shift_neg <- shift[which(neg)]
        shift_pos <- shift[which(pos)]
        x[neg] <- shift_left(x[neg], shift_neg)
        x[pos] <- shift_right(x[pos], shift_pos)
    }
    else {
        x[neg] <- shift_left(x[neg], shift)
        x[pos] <- shift_right(x[pos], shift)
    }
    return(x)
}

#' @name shift_left
#' @title shift_left from plyranges package
#' @description
#'
#' works on genomicranges
#'
#' @return granges
#' @author Stuart Lee
#' @author Michael Lawrence
#' @author Dianne Cook
#' @export
shift_left = function (x, shift = 0L)
{
    shift_l <- -1L * shift
    return(GenomicRanges::shift(x, shift_l))
}

#' @name shift_right
#' @title shift_right from plyranges package
#' @description
#'
#' works on genomicranges
#'
#' @return granges
#' @author Stuart Lee
#' @author Michael Lawrence
#' @author Dianne Cook
#' @export
shift_right = function (x, shift = 0L)
{
    return(GenomicRanges::shift(x, shift))
}

#' @name readinfasta
#' @title wrapper around readDNAStringSet to remove extraneous characters
#' @description
#'
#'
#' Uses Biostrings::readDNAStringSet to read in fasta
#' and remove all characters that do not correspond to rname
#'
#' @return DNAStringSet object
#' @author Kevin Hadi
#' @export
readinfasta = function(fa, allow_vertbar = FALSE) {
    if (!allow_vertbar)
        ptrn = " [ 0-9A-Za-z.\\/\\-\\(\\):,\\|_+\\[\\]]+$"
    else
        ptrn = " [ 0-9A-Za-z.\\/\\-\\(\\):,_+\\[\\]]+$"
    fa = Biostrings::readDNAStringSet(fa)
    names(fa) = sub(ptrn, "", khtools::names2(fa), perl = T)
    return(fa)
}

#' @name subgr
#' @title subgr
#'
#' @description
#'
#'
#'
#'
#' @return granges
#' @author Kevin Hadi
#' @export
subgr = function(x, y) {
    expr = as.expression(substitute(y))
    pf = parent.frame()
    ## pf2 = parent.frame(2)
    pf2 = stackenv2()
    return(x[S4Vectors:::safeEval(substitute(expr), S4Vectors::as.env(x, pf), pf2)])
    ## x[S4Vectors::with(x, eval(expr, enclos = pf))]
}

#' @name %Q%
#' @title query
#'
#'
#' @description
#'
#'
#'
#'
#' @return granges
#' @author Kevin Hadi
#' @export
"%Q%" = function(x,y, ...) {
    UseMethod("%Q%")
}

#' @name %Q%.GRanges
#' @title query on GRangesList
#'
#'
#' @description
#'
#'
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export
"%Q%.GRanges" = subgr

#' @name %Q%.GRangesList
#' @title query on GRangesList
#'
#' @description
#'
#'
#'
#' @return GRangesList
#' @author Kevin Hadi
#' @export
"%Q%.GRangesList" = subgr

#' @name %Q%.CompressedGRangesList
#' @title query on CompressedGRangesList
#'
#' @description
#'
#'
#'
#' @return GRangesList
#' @author Kevin Hadi
#' @export
"%Q%.CompressedGRangesList" = subgr


#' @name gr.genome
#' @title create GRanges of full genome coordinates
#' @description
#'
#' Grabs *.chrom.sizes file from
#' environmental variable "DEFAULT_GENOME" or
#' "DEFAULT_BSGENOME"
#'
#' May need to set either of these via
#' Sys.setenv(DEFAULT_BSGENOME = "path_to_ref.chrom.sizes")
#' Sys.setenv(DEFAULT_GENOME = "path_to_ref.chrom.sizes")
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.genome
gr.genome = function(si, onlystandard = TRUE, genome = NULL) {
    if (missing(si)) {
        gr = si2gr(hg_seqlengths(include.junk = !onlystandard, genome = genome))
    } else {
        gr = si2gr(si)
    }
    if (onlystandard) gr = keepStandardChromosomes(gr, pruning.mode = "coarse")
    gr.sort(gr)
}

#' @name gr.fixseq
#' @title get permissive seqlengths from multiple GRanges-like objects
#' @description
#'
#' takes seqlengths
#'
#' May need to set either of these via
#' Sys.setenv(DEFAULT_BSGENOME = "path_to_ref.chrom.sizes")
#' Sys.setenv(DEFAULT_GENOME = "path_to_ref.chrom.sizes")
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.fixseq
gr.fixseq = function(...) {
    output = Reduce(function(x, y) {
        if (!inherits(x, "data.frame")) x = within(enframe(x), {ord = seq_along(name)})
        if (!inherits(y, "data.frame")) y = enframe(y)
        df = merge(x, y, by = "name", all = T)
        df = df[order(df$ord),]
        out = data.frame(seqname = df$name, seqlength = pmax(df[['value.x']], df[['value.y']], na.rm = T))
        out
    }, lapply(list(...), seqlengths))
    with(output, setNames2(seqlength, seqname))
}


#' @name conform_si
#' @title force a ranges to conform to a new seqinfo
#' 
#' @description
#'
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export conform_si
conform_si = function(x, si) {
    ans = copy3(x)
    osi = seqinfo(x)
    osn = as.character(seqnames(x))
    newslev = union(seqlevels(si), seqlevels(osi))
    ## newsle = seqlengths(osi)
    new = rn2col(as.data.frame(si[newslev]), "seqnames")
    old = rn2col(as.data.frame(osi[newslev]), "seqnames")
    newsi = merge.repl(
        new, old, force_y = FALSE, overwrite_x = FALSE,
        by = "seqnames", keep_order = T, all = T
    )
    newsi = as(col2rn(asdf(newsi), "seqnames"), "Seqinfo")
    ans@seqnames = Rle(factor(osn, seqlevels(newsi)))
    ans@seqinfo = newsi
    return(ans)
}


#' @name gr.patch
#' @title same as gr.fix basically
#' @description
#'
#' need to change implementation...
#' it doesn't add to gr.fix so far
#' except to add default genome seqlengths
#' on top of whatever is already part of the input
#'
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.patch
gr.patch = function(gr, patch, onlystandard = TRUE) {
    if (missing(patch)) {
        patch = gr.genome(onlystandard = onlystandard)
    }
    sl = gr.fixseq(gr, patch)
    gr.fix(gr, sl)
}

#' @name gr2df
#' @title granges to datatable via dataframe
#' @description
#'
#' GRangesList
#'
#' @return GRangesList
#' @author Kevin Hadi
#' @export gr2df
gr2df = function(gr, var = "rowname") {
    sf = options()$stringsAsFactors
    on.exit({options(stringsAsFactors = sf)})
    options(stringsAsFactors = FALSE)
    rn = names(gr)
    if (!is.null(rn))
        rn2 = make.unique(rn)
    else
        rn2 = as.character(seq_along(gr))
    if (inherits(gr, "GRanges")) {
        df = GenomicRanges::as.data.frame(gr, row.names = rn2)
        cmd = sprintf("cbind(%s = rn2, df)", var)
        df = et(cmd)
    } else if (inherits(gr, "GRangesList"))
        df = GenomicRanges::as.data.frame(gr)
    setDT(df)
    return(dt_f2char(df,c("seqnames", "strand")))
}


#' @name grl.undf
#' @title grangeslist to data table via dataframe
#' @description
#'
#' GRangesList
#'
#' @return data table
#' @author Kevin Hadi
#' @export grl.undf
grl.undf = function(grl) {
    sf = options()$stringsAsFactors
    on.exit({options(stringsAsFactors = sf)})
    options(stringsAsFactors = FALSE)
    gr = gr2df(grl)
    lst = rleseq(gr$group, clump = FALSE)
    mc = setDT(GenomicRanges::as.data.frame(mcols(grl)[lst$idx,,drop = FALSE]))
    names(lst) = c("grl.ix", "grl.iix", "grl.len")
    return(dedup.cols(cbind(gr, mc, as.data.frame(lst))))
}

#' @name asdt
#' @title coerce to data table via setDT
#' @description
#'
#'
#'
#' @return data table
#' @author Kevin Hadi
#' @export
asdt = function(obj) {
    out = setDT(as.data.frame(obj))
    return(out)
}

#' @name asdf
#' @title coerce to data frame
#' @description
#'
#'
#'
#' @return data frame
#' @author Kevin Hadi
#' @export
asdf = function(obj) {
    return(as.data.frame(obj))
}

#' @name asm
#' @title coerce to matrix
#' @description
#'
#'
#'
#' @return matrix
#' @author Kevin Hadi
#' @export
asm = function(obj) {
    return(as.matrix(obj))
}

#' @name grl.flipstrand
#' @title flip strand of grangeslist
#' @description
#'
#' GRangesList
#'
#' @return GRangesList
#' @author Kevin Hadi
#' @export grl.flipstrand
grl.flipstrand = function(grl) {
    tmp_vals = mcols(grl)
    tmp_gr = unlist(grl, use.names = FALSE)
    tmp_gr = gr.flipstrand(tmp_gr)
    new_grl = relist(tmp_gr, grl)
    mcols(new_grl) = tmp_vals
    return(new_grl)
}


#' @name gr.strand
#' @title specify strand of granges or grangeslist
#' @description
#'
#'
#' @return granges or grangeslist
#' @author Kevin Hadi
#' @export gr.strand
gr.strand = function(gr, str = "*") {
    strand(gr) = str
    return(gr)
}



#' @name df2gr
#' @title data frame to GRanges
#' @description
#'
#' data frame to GRanges
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export df2gr
df2gr = function (df, seqnames.field = "seqnames", start.field = "start", 
    end.field = "end", strand.field = "strand", ignore.strand = FALSE, 
    keep.extra.columns = TRUE, starts.in.df.are.0based = FALSE) {
    if (!inherits(df, "data.frame")) {
        df = as.data.frame(df)
    }
    if (inherits(seqnames.field, c("numeric", "integer"))) {
        seqnames.field = colnames(df)[seqnames.field]
    }
    if (inherits(start.field, c("numeric", "integer"))) {
        start.field = colnames(df)[start.field]
    }
    if (inherits(end.field, c("numeric", "integer"))) {
        end.field = colnames(df)[end.field]
    }
    if (inherits(strand.field, c("numeric", "integer"))) {
        strand.field = colnames(df)[strand.field]
    }
    badcols = c("seqnames", "start", "end", "strand", "ranges", 
        "width", "element", "seqlengths", "seqlevels", "isCircular")
    badcols = setdiff(badcols, c(seqnames.field, start.field, 
        end.field, strand.field))
    ix = which(colnames(df) %in% badcols)
    if (length(ix) > 0) 
        df = et(sprintf("df[, -%s]", mkst(ix)))
    cnames = colnames(df)
    names(cnames) = cnames
    relevant_cols = match(c(seqnames.field, start.field, end.field, 
        strand.field), cnames)
    if (is.na(relevant_cols[4])) {
        relevant_cols = relevant_cols[-4]
        ignore.strand = TRUE
    }
    addon = rand.string()
    cnames[relevant_cols] = paste(cnames[relevant_cols], addon, 
        sep = "_")
    colnames(df)[relevant_cols] = cnames[relevant_cols]
    seqnames.field = paste(seqnames.field, addon, sep = "_")
    start.field = paste(start.field, addon, sep = "_")
    end.field = paste(end.field, addon, sep = "_")
    strand.field = paste(strand.field, addon, sep = "_")
    makeGRangesFromDataFrame(df, seqnames.field = seqnames.field, 
        start.field = start.field, end.field = end.field, strand.field = strand.field, 
        ignore.strand = ignore.strand, keep.extra.columns = keep.extra.columns, 
        starts.in.df.are.0based = starts.in.df.are.0based)
}

#' @name df2grl
#' @title data frame to GRangesList
#' @description
#'
#' data frame to grl
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export df2grl
df2grl = function(df,
                  seqnames.field = "seqnames",
                  start.field = "start",
                  end.field = "end",
                  strand.field = "strand",
                  split.field = "grl.ix",
                  ignore.strand = FALSE,
                  keep.extra.columns = TRUE,
                  asmcols = NULL,
                  keepgrmeta=FALSE) {
    if (!inherits(df, "data.frame")) {
        df = as.data.frame(df)
    }
    if (inherits(seqnames.field, c("numeric", "integer"))) {
        seqnames.field = colnames(df)[seqnames.field]
    }
    if (inherits(start.field, c("numeric", "integer"))) {
        start.field = colnames(df)[start.field]
    }
    if (inherits(end.field, c("numeric", "integer"))) {
        end.field = colnames(df)[end.field]
    }
    if (inherits(strand.field, c("numeric", "integer"))) {
        strand.field = colnames(df)[strand.field]
    }
    if (inherits(split.field, c("numeric", "integer"))) {
        split.field = copy2(colnames(df)[split.field])
    }
    if (!is.null(asmcols) && inherits(asmcols, c("numeric", "integer"))) {
        asmcols = colnames(df)[asmcols]
    }
    o.split.field = copy2(split.field)
    badcols = c("seqnames", "start", "end", "strand", "ranges", "width", "element", "seqlengths", "seqlevels", "isCircular")
    badcols = setdiff(badcols, c(seqnames.field, start.field, end.field, strand.field, split.field))
    ix = which(colnames(df) %in% badcols)
    if (length(ix) > 0) df = et(sprintf("df[, -%s]", mkst(ix)))
    cnames = colnames(df) # makeGRangesFromDataFrame makes use of tolower()
    names(cnames) = cnames
    ## cnames = tolower(cnames)
    ## relevant_cols = which(cnames %in% c(seqnames.field, start.field, end.field, strand.field))
    ## relevant_cols[duplicated(cnames[relevant_cols])]
    ## num = rleseq(cnames[relevant_cols], clump = T)$seq
    ## deduped = paste0(names(cnames[relevant_cols]),
    ##                  ifelse(num > 1, paste0(".", as.character(num)), ""))
    ## names(cnames)
    relevant_cols = match(c(seqnames.field, start.field, end.field, strand.field, split.field), cnames)
    if (is.na(relevant_cols[4])) {
        relevant_cols = relevant_cols[-4]
        ignore.strand = TRUE
    }
    if (is.na(relevant_cols[5])) {
        relevant_cols = relevant_cols[-5]
    }
    addon = rand.string()
    cnames[relevant_cols] = paste(cnames[relevant_cols], addon, sep = "_")
    colnames(df)[relevant_cols] = cnames[relevant_cols]
    seqnames.field = paste(seqnames.field, addon, sep = "_")
    start.field = paste(start.field, addon, sep = "_")
    end.field = paste(end.field, addon, sep = "_")
    strand.field = paste(strand.field, addon, sep = "_")
    split.field = paste(split.field, addon, sep = "_")
    spl = df[[split.field]]
    tmpix = which(!duplicated(spl))
    if (!is.null(asmcols)) {
        attach_to_mcols = qmat(df,cid = asmcols)
        ## rows = which(!duped(attach_to_mcols))
        rows = tmpix
        ## ix = do.call(rleseq, list(attach_to_mcols, clump = TRUE))
        if (!keepgrmeta)
            df = qmat(df, ,-which(colnames(df) %in% asmcols))
    }
    out = makeGRangesListFromDataFrame(df,
                                       seqnames.field = seqnames.field,
                                       start.field = start.field,
                                       end.field = end.field,
                                       strand.field = strand.field,
                                       split.field = split.field,
                                       ignore.strand = ignore.strand,
                                       keep.extra.columns = keep.extra.columns)
    
    mcols(out)[[o.split.field]] = spl[tmpix]
    if (!is.null(asmcols)) {
        mcols(out) = cbind(mcols(out), qmat(attach_to_mcols, rows))
    }
    return(out)
}


#' @name gr.round
#' @title round window to nearest unit
#' @description
#'
#' Rounding window to nearest unit.
#' meant to be used for plotting window in gTrack.
#' For that purpose, it's most useful to use reduce = TRUE
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.round
gr.round = function(gr, nearest = 1e4, all = TRUE, reduce = FALSE) {
  if (reduce)
    gr = GenomicRanges::reduce(gr)
  wid = round((width(gr) + (0.5 * nearest)) / nearest) * nearest
  if (all) {
    wid = max(wid)
  }
  out = gr.resize(gr, wid = wid, pad = FALSE, reduce = T)
  return(out)
}


#' @name gr.sort
#' @title sort granges, grangeslist
#' @description
#'
#' sort granges or grangeslist by seqlevels
#' also reorders seqlevels into 1:22, X, Y format
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.sort
gr.sort = function(gr, ignore.strand = TRUE) {
    return(sort(sortSeqlevels(gr), ignore.strand = ignore.strand))
}

#' @name gr.order
#' @title order granges, grangeslist
#' @description
#'
#' order granges or grangeslist by seqlevels
#' also reorders seqlevels into 1:22, X, Y format
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.order
gr.order = function(gr, ignore.strand = T) {
    sgr = GenomeInfoDb::sortSeqlevels(gr)
    if (isTRUE(ignore.strand))
        sgr = gr.stripstrand(sgr)
    return(GenomicRanges::order(sgr))
}


#' @name gr.resize
#' @title Resize granges without running into negative width error
#' @description
#'
#' lower size limit of window is 0
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.resize
gr.resize = function (gr, width, pad = TRUE, minwid = 0, each = TRUE, ignore.strand = FALSE,
    fix = "center", reduce = FALSE)
{
    wid = width
    if (pad) {
        if (isTRUE(each)) {
            wid = wid * 2
        }
        width.arg = pmax(width(gr) + wid, minwid)
    }
    else width.arg = pmax(wid, minwid)
    if (reduce) {
        ## gr = GenomicRanges::reduce(gr + width.arg, ignore.strand = ignore.strand) -
        ##     width.arg
        out = gr.resize(gr, width, pad = pad, minwid = minwid, each = each, ignore.strand = ignore.strand, fix = fix, reduce = FALSE)
        out = GenomicRanges::reduce(out, ignore.strand = ignore.strand)
        out = gr.resize(out, width, pad = pad, minwid = minwid, each = each, ignore.strand = ignore.strand, fix = fix, reduce = FALSE)
        return(out)
    }
    return(GenomicRanges::resize(gr, width = width.arg, fix = fix,
        ignore.strand = ignore.strand))
}




#' @name parse.gr2
#' @title a robust parse.gr
#' @description
#'
#' version of parse.gr that is able to convert ranges with minus signs
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export parse.gr2
parse.gr2 = function(...) {
    grl.unlist(parse.grl2(...))
}


#' @name parse.grl2
#' @title a robust parse.grl
#' @description
#'
#' version of parse.grl that is able to convert ranges with minus signs
#'
#' @param str A string that can be parsed as ranged data "A:1-10+"
#' @param meta A table that will be added as metadata
#' @return GRangesList
#' @author Kevin Hadi
#' @export parse.grl2
parse.grl2 = function (str, meta = NULL, fixna = FALSE) 
{
    tmp = stringi::stri_split_regex(str, pattern = ",|;")
    grl.ix = rep(seq_along(tmp), lengths(tmp))
    tmp = unlist(tmp)
    if (fixna) {
        wasna = is.na(tmp)
        tmp = ifelse(wasna, "1:0--1", tmp)
    }
    tmp = gsub("(\\w+)(:)([[:punct:]]?\\w+)(-)([[:punct:]]?\\w+)([[:punct:]]?)", 
        "\\1 \\2 \\3 \\4 \\5 \\6", tmp)
    mat = stringi::stri_split_fixed(tmp, pattern = " ", simplify = "TRUE")
    gr = GRanges(seqnames = mat[, 1], ranges = IRanges(as.integer(mat[, 
        3]), as.integer(mat[, 5])), strand = ifelse(nchar(mat[, 
        6]) == 0 | !mat[, 6] %in% c("+", "-"), "*", mat[, 6]))
    if (fixna) {
        gr$was_na = wasna
    }
    gr = GenomicRanges::split(gr, grl.ix)
    if (!is.null(meta) && nrow(meta) == length(gr)) 
        S4Vectors::values(gr) = meta
    return(gr)
}




## parse.grl2 = function(str, meta = NULL) {
##     ## library(stringi)
##     tmp = stringi::stri_split_regex(str, pattern = ",|;")
##     grl.ix = rep(seq_along(tmp), lengths(tmp))
##     tmp = unlist(tmp)
##     tmp = gsub("(\\w+)(:)([[:punct:]]?\\w+)(-)([[:punct:]]?\\w+)([[:punct:]]?)", "\\1 \\2 \\3 \\4 \\5 \\6", tmp)
##     mat = stringi::stri_split_fixed(tmp, pattern = " ", simplify = "TRUE")
##     gr = GRanges(seqnames = mat[,1],
##                  ranges = IRanges(as.integer(mat[,3]), as.integer(mat[,5])),
##                  strand = case_when(nchar(mat[,6]) == 0 | !mat[,6] %in% c("+", "-") ~ "*",
##                                     TRUE ~ mat[,6]),
##                  grl.ix = grl.ix)
##     gr = gr.noval(split(gr, gr$grl.ix))
##     if (!is.null(meta) && nrow(meta) == length(gr))
##         values(gr) = meta
##     return(gr)
## }

#' @name gr_calc_cov
#' @title output data structure for ski slope from anchorlifted SNV
#'
#'
#' @return A GRanges
#' @export gr_calc_cov
gr_calc_cov = function(gr, PAD = 50, field = NULL, start.base = -1e6, end.base = -5e3, win = 1e4, FUN = "mean", baseline = NULL, normfun = "*", normfactor = NULL) {
    `%&%` = gUtils::`%&%`
    if (inherits(gr, "data.frame")) {
        gr = dt2gr(gr)
    }
    win = GRanges("Anchor", IRanges(-abs(win), abs(win)))
    ## silent({library(plyranges); forceload()})
    grcov = gUtils::gr.sum(gr + PAD, field = field)
    if (!is.null(field))
        grcov = grcov %>% select(score = !!field)
    ## mcols(grcov)[["score"]] = pmax(0, mcols(grcov)[["score"]], 0)
    ## grcov2 = gr.tile(grcov, 1)
    grcov2 = gr.tile(GRanges("Anchor", IRanges(-1e6, 1e6)) + PAD, 1)
    ## grcov2 = gr.tile(GRanges("Anchor", IRanges(start(head(grcov, 1)), end(tail(grcov, 1)))), 1)
    if (!is.empty(grcov)) {
        ## grcov2$score = gr.eval(grcov2, grcov, score, 0)
        grcov2 = within(plyranges::join_overlap_left(grcov2, grcov), {score = replace_na(score, 0)})
        ## grcov2 = grcov2 %$% grcov
        ## grcov2$score = grcov2$score %>% replace_na(0)
    } else {
        grcov2$score = 0
    }
    if (!is.null(normfactor)) {
        if (!(length(normfactor) == length(grcov2) | length(normfactor == 1)))
            stop("normfactor needs to be same length")
        grcov2$score = get(normfun)(grcov2$score, normfactor)
    }
    if (is.null(baseline)) {
        baseline = with(grcov2, {
            ## this_subset = data.table::between(start, (abs(start.base) + PAD) * sign(start.base), ((abs(end.base) + PAD) * sign(end.base)) - 1)
            this_subset = data.table::between(start, start.base - PAD, end.base + PAD - 1)
            get(FUN)(score[this_subset])
            ## sum(score[this_subset] * width[this_subset]) / sum(width[this_subset])
        })
    }
    ## baseline = gr2dt(grcov2)[data.table::between(start, (abs(start.base) + PAD) * sign(start.base), ((abs(end.base) + PAD) * sign(end.base)) - 1)][, sum(score * width) / sum(width)]
    score = grcov2$score
    if (!(length(baseline) == length(score) | length(baseline == 1)))
        stop("baseline needs to be same length as score or a length 1 vector")
    rel = pmax(score, 0) / (baseline + 1e-12)
    ## grcov2$rel = (grcov2$score) / (baseline + 1e-12)
    grcov2$score = rel
    grcov2$baseline = baseline
    grcov2 %&% win
}


#' @export std.calc.cov
std.calc.cov = function(anci, pad, field = NULL, baseline = NULL, FUN = "median") {
    gr_calc_cov(anci %>% dt2gr, PAD = pad, start.base = -5e3, end.base = 0, FUN = FUN, field = field, win = 5e3, baseline = baseline)
}


#' @name gr.disjoin
#' @title updated gr.disjoin from gUtils to work with GRangesList
#' @description
#'
#'
#'
#' @return GRanges or GRangesList
#' @author Kevin Hadi
#' @export gr.disjoin
gr.disjoin = function (x, ..., ignore.strand = TRUE)
{
    if (inherits(x, "GRangesList")) {
        gr = GenomicRanges:::deconstructGRLintoGR(x)
        if (ignore.strand) gr = gr.stripstrand(gr)
        return(GenomicRanges:::reconstructGRLfromGR(gUtils::gr.disjoin(gr, ..., ignore.strand = ignore.strand), x))
    }
    y = disjoin(x, ...)
    ix = gr.match(y, x, ignore.strand = ignore.strand)
    values(y) = values(x)[ix, , drop = FALSE]
    return(y)
}



#' @name grl.disjoin
#' @title disjoin on grangeslist
#' @description
#'
#'
#'
#' @return GRangesList
#' @author Kevin Hadi
#' @export grl.disjoin
grl.disjoin = function(x, ..., ignore.strand = T) {
  gr = GenomicRanges:::deconstructGRLintoGR(x)
  if (ignore.strand) gr = gr.stripstrand(gr)
  GenomicRanges:::reconstructGRLfromGR(gr.disjoin(gr, ..., ignore.strand = ignore.strand), x)
}


#' @name gr.split
#' @title split a gr by field(s) in elementMetadata of GRanges or a given vector
#' @description
#'
#' split GRanges by field(s)
#' if providing a variable not already within the GRanges,
#' may need to use dynget(variable_name)
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.split
gr.split = function(gr, ..., sep = paste0(" ", rand.string(length = 8), " "), addmcols = TRUE) {
  lst = as.list(match.call())[-1]
  ix = which(!names(lst) %in% c("gr", "sep", "addmcols"))
  tmpix = eval(quote(do.call(paste, c(lst[ix], alist(sep = sep)))), S4Vectors::as.env(mcols(gr), environment()), parent.frame())
  ## tmpix = with(as.list(mcols(gr)), do.call(paste, c(lst[ix], alist(sep = sep))))
  uix = which(!duplicated(tmpix))
  tmpix = factor(tmpix, levels = tmpix[uix])
  grl = gr %>% GenomicRanges::split(tmpix)
  these = unlist(strsplit(toString(lst[ix]), ", "))
  if (addmcols) {
    grl@elementMetadata = mcols(gr)[uix, these,drop = F]
  }
  return(grl)
}

#' @name gr.spreduce
#' @title reduce based on a field(s) to split by in elementMetadata of GRanges, or given vector
#' @description
#'
#' split and reduce GRanges by field(s)
#' if providing a variable not already within the GRanges,
#' may need to use dynget(variable_name)
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.spreduce
gr.spreduce = function(gr,  ..., ignore.strand = FALSE, pad = 0, return.grl = FALSE, sep = paste0(" ", rand.string(length = 8), " ")) {
  lst = as.list(match.call())[-1]
  ix = which(!names(lst) %in% c("gr", "sep", "pad", "ignore.strand", "return.grl"))
  vars = unlist(sapply(lst[ix], function(x) unlist(sapply(x, toString))))
  if (length(vars) == 1) {
    if (!vars %in% colnames(mcols(gr)))
      vars = tryCatch(unlist(list(...)), error = function(e) vars)
  }
  if (!all(vars %in% colnames(mcols(gr))))
    stop("Must specify valid metadata columns in gr")
  tmpix = do.call(
    function(...) paste(..., sep = sep),
    as.list(mcols(gr)[,vars, drop = F]))
  unix = which(!duplicated(tmpix))
  tmpix = factor(tmpix, levels = tmpix[unix])
  grl = unname(gr.noval(gr) %>% GenomicRanges::split(tmpix))
  grl = GenomicRanges::reduce(grl + pad, ignore.strand = ignore.strand)
  if (return.grl) {
    mcols(grl) = mcols(gr)[unix,vars,drop = F]
    return(grl)
  } else {
    out = unlist(grl)
    mcols(out) = mcols(gr)[rep(unix, times = IRanges::width(grl@partitioning)),
      vars,drop = F]
    return(out)
  }
}

#' @name gr.sprange
#' @title get range based on a field(s) to split by in elementMetadata of GRanges, or given vector
#' @description
#'
#' split and get range of GRanges by field(s)
#' if providing a variable not already within the GRanges,
#' may need to use dynget(variable_name)
#'
#' @return GRanges
#' @author Kevin Hadi
#' @export gr.sprange
gr.sprange = function (gr, ..., ignore.strand = FALSE, pad = 0, return.grl = FALSE, 
    sep = paste0(" ", rand.string(length = 8), " ")) 
{
    lst = as.list(match.call())[-1]
    ix = which(!names(lst) %in% c("gr", "sep", "pad", "ignore.strand", 
        "return.grl"))
    vars = unlist(sapply(lst[ix], function(x) unlist(sapply(x, 
        toString))))
    if (length(vars) == 1) {
        if (!vars %in% colnames(mcols(gr))) 
            vars = tryCatch(unlist(list(...)), error = function(e) vars)
    }
    if (!all(vars %in% colnames(mcols(gr)))) 
        stop("Must specify valid metadata columns in gr")
    tmpix = do.call(function(...) paste(..., sep = sep), as.list(mcols(gr)[, 
        vars, drop = F]))
    unix = which(!duplicated(tmpix))
    tmpix = factor(tmpix, levels = tmpix[unix])
    grl = unname(gr.noval(gr) %>% GenomicRanges::split(tmpix))
    grl = range(grl + pad, ignore.strand = ignore.strand)
    if (return.grl) {
        mcols(grl) = mcols(gr)[unix, vars, drop = F]
        return(grl)
    }
    else {
        out = unlist(grl)
        mcols(out) = mcols(gr)[rep(unix, times = IRanges::width(grl@partitioning)), 
            vars, drop = F]
        return(out)
    }
}


## gr.spreduce = function(gr,  ..., pad = 0, sep = paste0(" ", rand.string(length = 8), " ")) {
##   lst = as.list(match.call())[-1]
##   ix = which(!names(lst) %in% c("gr", "sep", "pad"))
##   tmpix = with(as.list(mcols(gr)), do.call(paste, c(lst[ix], alist(sep = sep))))
##   tmpix = factor(tmpix, levels = unique(tmpix))
##   grl = gr %>% GenomicRanges::split(tmpix)
##   dt = as.data.table(GenomicRanges::reduce(grl + pad))
##   nmix = which(unlist(lapply(lst[ix], function(x) is.name(x) & !is.call(x))))
##   nm = lapply(lst[ix], toString)
##   rmix = which(unlist(nm) %in% colnames(dt))
##   nm[rmix] = list(character(0))
##   if (length(rmix))
##     nmix = nmix[-rmix]
##   ## nm[lengths(nm) == 0] = list(character(0))
##   ## nm[-nmix] = character(0)
##   nm[-nmix] = list(character(0))
##   ## nmix = which(!nm == "NULL")
##   dt = dt[, cbind(.SD, setnames(as.data.table(data.table::tstrsplit(group_name, split = sep)), nmix, unlist(nm)))][, group_name := NULL]
##   return(dt2gr(dt))
## }


#' @name gr.noval
#' @title get rid of mcols on GRanges/GRangesLists
#' @description
#'
#' remove all metadata from GRanges or GRangesList
#'
#' @return GRanges or GRangesList
#' @author Kevin Hadi
#' @export gr.noval
gr.noval = function(gr, keep.col = NULL, drop.col = NULL) {
    if (is.null(keep.col) & is.null(drop.col)) {
        select_col = NULL
    } else {
        all_col = colnames(gr@elementMetadata)
        if (inherits(gr, "GRangesList")) {
            all_col = c(all_col, colnames(gr@unlistData@elementMetadata))
        }

        if (!is.null(keep.col) & is.null(drop.col)) {
            select_col = intersect(all_col, keep.col)
        } else if (is.null(keep.col) & !is.null(drop.col)) {
            select_col = setdiff(all_col, drop.col)
        } else if (!is.null(keep.col) && !is.null(drop.col)) {
            if (intersect(keep.col, drop.col) > 0) {
                warning("drop.col and keep.col args have overlapping elements\nkeeping the columns that overlap")
                select_col = intersect(setdiff(all_col, setdiff(drop.col, keep.col)), keep.col)
            }
        }
    }
    if (inherits(gr, "GRangesList")) {
        tmp_query = intersect(select_col, colnames(gr@unlistData@elementMetadata))
        gr@unlistData@elementMetadata = gr@unlistData@elementMetadata[,c(tmp_query), drop = FALSE]
    }
    tmp_query = intersect(select_col, colnames(gr@elementMetadata))
    gr@elementMetadata = gr@elementMetadata[,c(tmp_query),drop = FALSE]
    return(gr)
}

#' @name gr.within
#' @title within on GRanges, S3
#' @description
#'
#'
#' @return GRanges
#' @rdname gr.within
#' @author Kevin Hadi
#' @export gr.within
gr.within = function(data, expr) {
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
    orig.names = setdiff(rev(names(e)), "X")
    rm(list = "X", envir = e)
    ## e$X = NULL
    e$data <- granges(data)
    e$seqnames = seqnames(e$data)
    e$start = start(e$data)
    e$end = end(e$data)
    e$strand = as.character(strand(e$data))
    e$width = as.integer(width(e$data))
    S4Vectors:::safeEval(substitute(expr), e, top_prenv1(expr))
    ch.fields = all.vars(substitute(expr))
    reserved <- c("seqnames", "start", "end", "width", "strand", "data")
    if ("seqnames" %in% ch.fields) seqnames(e$data) = Rle(factor(e$seqnames, levels = levels(seqnames(e$data))))
    if ("width" %in% ch.fields) width(e$data) = e$width
    if ("strand" %in% ch.fields) strand(e$data) = e$strand
    if (any(c("start", "end") %in% ch.fields)) e@ranges = IRanges(e$start, e$end)
    data = e$data
    l <- mget(setdiff(ls(e), reserved), e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(colnames(mcols(data)), (nl <- names(l))))
    tmp = as(l, "DataFrame")
    newn = unlist(lapply(as.list(substitute(expr))[-1], function(x) {
        x = as.character(x)
        if (x[1] == "=")
            return(x[2])
        else
            return(NULL)
    }))
    neword = union(union(orig.names, newn), colnames(tmp))
    mcols(data) = tmp[,na.omit(match3(neword, colnames(tmp))), drop = FALSE]
    if (nD) {
        for (nm in del)
            mcols(data)[[nm]] = NULL
    }
    ## if (!identical(granges(data), e$data)) {
    ##     granges(data) <- e$data
    ## }
    return(data)
}

gr.within2 = function(data, expr) {
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
    orig.names = setdiff(rev(names(e)), "X")
    rm(list = "X", envir = e)
    ## e$X = NULL
    e$data <- granges(data)
    e$seqnames = as.integer(seqnames(e$data))
    e$start = start(e$data)
    e$end = end(e$data)
    e$strand = as.character(strand(e$data))
    e$width = as.integer(width(e$data))
    S4Vectors:::safeEval(substitute(expr, parent.frame()), e, top_prenv1(expr))
    ch.fields = all.vars(substitute(expr, parent.frame()))
    reserved <- c("seqnames", "start", "end", "width", "strand", "data")
    if ("seqnames" %in% ch.fields) seqnames(e$data) = Rle(factor(e$seqnames, levels = levels(seqnames(e$data))))
    if ("width" %in% ch.fields) width(e$data) = e$width
    if ("strand" %in% ch.fields) strand(e$data) = e$strand
    if (any(c("start", "end") %in% ch.fields)) e@ranges = IRanges(e$start, e$end)
    data = e$data
    l <- mget(setdiff(ls(e), reserved), e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(colnames(mcols(data)), (nl <- names(l))))
    tmp = as(l, "DataFrame")
    newn = unlist(lapply(as.list(substitute(expr, parent.frame()))[-1], function(x) {
        x = as.character(x)
        if (x[1] == "=")
            return(x[2])
        else
            return(NULL)
    }))
    neword = union(union(orig.names, newn), colnames(tmp))
    mcols(data) = tmp[,na.omit(match3(neword, colnames(tmp))), drop = FALSE]
    if (nD) {
        for (nm in del)
            mcols(data)[[nm]] = NULL
    }
    ## if (!identical(granges(data), e$data)) {
    ##     granges(data) <- e$data
    ## }
    return(data)
}


setGeneric('within')

#' @name within
#' @title within on GRanges
#' @description
#'
#'
#' @return GRanges
#' @rdname gr_within
#' @exportMethod within
#' @aliases within,GRanges-method
#' @author Kevin Hadi
#' @export
setMethod("within", signature(data = "GRanges"), NULL)
setMethod("within", signature(data = "GRanges"), gr.within2)


tmpgrlwithin = function(data, expr) {
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
    orig.names = setdiff(rev(names(e)), "X")
    rm(list = "X", envir = e)
    ## e$X = NULL
    e$grangeslist <- gr.noval(data)
    S4Vectors:::safeEval(substitute(expr, parent.frame()), e, top_prenv1(expr))
    ## reserved <- c("ranges", "start", "end", "width", "space")
    reserved <- c("seqnames", "start", "end", "width", "strand", "granges", "grangeslist")
    l <- mget(setdiff(ls(e), reserved), e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(colnames(mcols(data)), (nl <- names(l))))
    tmp = as(l, "DataFrame")
    newn = unlist(lapply(as.list(substitute(expr, parent.frame()))[-1], function(x) {
        x = as.character(x)
        if (x[1] == "=")
            return(x[2])
        else
            return(NULL)
    }))
    neword = union(union(orig.names, newn), colnames(tmp))
    mcols(data) = tmp[,na.omit(match3(neword, colnames(tmp))), drop = FALSE]
    ## mcols(data) = as(l, "DataFrame")
    if (nD) {
        for (nm in del)
            mcols(data)[[nm]] = NULL
    }
    if (!identical(gr.noval(data), e$grangeslist)) {
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
}


setMethod("within", signature(data = "CompressedGRangesList"), NULL)
setMethod("within", signature(data = "GRangesList"), NULL)
setMethod("within", signature(data = "CompressedGRangesList"), tmpgrlwithin)
setMethod("within", signature(data = "GRangesList"), tmpgrlwithin)


setMethod("within", signature(data = "IRanges"), NULL)
setMethod("within", signature(data = "IRanges"), function(data, expr) {
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
    e$data <- ranges(data)
    e$start = start(e$data)
    e$end = end(e$data)
    e$width = as.integer(width(e$data))
    S4Vectors:::safeEval(substitute(expr, parent.frame()), e, top_prenv1(expr))
    reserved <- c("start", "end", "width", "data")
    l <- mget(setdiff(ls(e), reserved), e)
    l <- l[!sapply(l, is.null)]
    nD <- length(del <- setdiff(colnames(mcols(data)), (nl <- names(l))))
    mcols(data) = as(l, "DataFrame")
    if (nD) {
        for (nm in del)
            mcols(data)[[nm]] = NULL
    }
    if (!identical(ranges(data), e$data)) {
        ranges(data) <- e$data
    }
    data
})

tmpgrlgaps = function(x, start = 1L, end = seqlengths(x)) {
  ## if (!is.null(names(start)))
  ##   start <- start[seqlevels]
  ## if (!is.null(names(end)))
  ##   end <- end[seqlevels]
  ## start <- S4Vectors:::recycleVector(start, length(seqlevels))
  ## start <- rep(start, each = 3L)
  ## end <- S4Vectors:::recycleVector(end, length(seqlevels))
  ## end <- rep(end, each = 3L)
  expand.levels = TRUE
  gr = GenomicRanges:::deconstructGRLintoGR(x, expand.levels = expand.levels)
  grlix = formatC(seq_along(x), width = floor(log10(length(x))) + 1, format = "d", flag = "0")
  snid = as.integer(seqnames(x@unlistData))
  if (isTRUE(expand.levels)) seql = seq_along(seqlevels(x)) else seql = unique(snid)
  slix = formatC(seql, width = floor(log10(max(seql))) + 1, format = "d", flag = "0")
  cdt = data.table::CJ(Var1 = grlix, Var2 = slix)[, oix := seq_len(.N)]
  cdt = merge(cdt, data.frame(sl = seqlengths(x)[seql], Var2 = slix), by = "Var2")
  setkey(cdt, oix)
  nseqlevels = cdt[, paste(Var1, Var2, sep = "|")]
  f1 = rep(grlix, lengths(x))
  ## f2 = slix[snid]
  f2 = setkey(data.table(seql, slix), seql)[list(snid)]$slix
  seqn = paste(f1, f2, sep = "|")
  seqlevels(gr) = c(nseqlevels)
  seqlengths(gr) = c(cdt$sl)
  seqnames(gr) = S4Vectors::Rle(factor(seqn, nseqlevels))
  rgl = GenomicRanges:::deconstructGRintoRGL(gr)
  ## rgl2 = gaps(rgl, start = rep(rep(start, cdt[,.N]), each = 3L), end = rep(cdt$sl, each = 3L))
  rgl2 = gaps(rgl, start = rep(rep(1, cdt[,.N]), each = 3L), end = rep(cdt$sl, each = 3L))
  GenomicRanges:::reconstructGRLfromGR(GenomicRanges:::reconstructGRfromRGL(rgl2, gr), x)
}


tmpgrlgaps2 = function(x, start = 1L, end = seqlengths(x), expand.levels = TRUE) {
  ## if (!is.null(names(start)))
  ##   start <- start[seqlevels]
  ## if (!is.null(names(end)))
  ##   end <- end[seqlevels]
  ## start <- S4Vectors:::recycleVector(start, length(seqlevels))
  ## start <- rep(start, each = 3L)
  ## end <- S4Vectors:::recycleVector(end, length(seqlevels))
  ## end <- rep(end, each = 3L)
  gr = GenomicRanges:::deconstructGRLintoGR(x, expand.levels = expand.levels)
  grlix = formatC(seq_along(x), width = floor(log10(length(x))) + 1, format = "d", flag = "0")
  snid = as.integer(seqnames(x@unlistData))
  if (isTRUE(expand.levels)) seql = seq_along(seqlevels(x)) else seql = unique(snid)
  slix = formatC(seql, width = floor(log10(max(seql))) + 1, format = "d", flag = "0")
  cdt = data.table::CJ(Var1 = grlix, Var2 = slix)[, oix := seq_len(.N)]
  cdt = merge(cdt, data.frame(sl = seqlengths(x)[seql], Var2 = slix), by = "Var2")
  setkey(cdt, oix)
  nseqlevels = cdt[, paste(Var1, Var2, sep = "|")]
  f1 = rep(grlix, lengths(x))
  ## f2 = slix[snid]
  f2 = setkey(data.table(seql, slix), seql)[list(snid)]$slix
  seqn = paste(f1, f2, sep = "|")
  seqlevels(gr) = c(nseqlevels)
  seqlengths(gr) = c(cdt$sl)
  seqnames(gr) = S4Vectors::Rle(factor(seqn, nseqlevels))
  rgl = GenomicRanges:::deconstructGRintoRGL(gr)
  ## rgl2 = gaps(rgl, start = rep(rep(start, cdt[,.N]), each = 3L), end = rep(cdt$sl, each = 3L))
  rgl2 = gaps(rgl, start = rep(rep(1, cdt[,.N]), each = 3L), end = rep(cdt$sl, each = 3L))
  GenomicRanges:::reconstructGRLfromGR(GenomicRanges:::reconstructGRfromRGL(rgl2, gr), x)
}


#' @name gaps
#' @title gaps on GRangesList
#' @description
#'
#'
#' @return GRangesList
#' @rdname grl_gaps
#' @exportMethod gaps
#' @aliases gaps,GRangesList-method
#' @author Kevin Hadi
#' @export
setMethod(f = "gaps", signature = signature(x = "CompressedGRangesList"), definition = NULL)
setMethod(f = "gaps", signature = signature(x = "GRangesList"), definition = NULL)
setMethod("gaps", signature(x = "GRangesList"), tmpgrlgaps)
setMethod("gaps", signature(x = "CompressedGRangesList"), tmpgrlgaps)

#' @name gr.splgaps
#' @title gaps on GRanges, splitting by values in a metadata field
#' @description
#'
#'
#' @return GRangesList
#' @rdname gr.splgaps
#' @author Kevin Hadi
#' @export gr.splgaps
gr.splgaps <- function(gr, ..., ignore.strand = TRUE, sep = paste0(" ", rand.string(length = 8), " "), start = 1L, end = seqlengths(gr), cleannm = TRUE, expand.levels = TRUE) {
  lst = as.list(match.call())[-1]
  ix = which(!names(lst) %in% c("gr", "sep", "cleannm", "start", "end", "expand.levels"))
  cl = sapply(lst[ix], class)
  vars = unlist(sapply(lst[ix], function(x) unlist(sapply(x, toString))))
  if (length(vars) == 1) {
    if (!vars %in% colnames(mcols(gr)))
      vars = tryCatch(unlist(list(...)), error = function(e) vars)
  }
  if (!all(vars %in% colnames(mcols(gr))))
    stop("Must specify valid metadata columns in gr")
  tmpix = S4Vectors::do.call(function(...) paste(..., sep = sep),
    mcols(gr)[,vars,drop = F])
  unix = which(!duplicated(tmpix))
  tmpix = factor(tmpix, levels = tmpix[unix])
  if (ignore.strand)
      gr = gr.stripstrand(gr)
  grl = gr.noval(gr) %>% GenomicRanges::split(tmpix)
  out = tmpgrlgaps2(grl, start = start, end = end, expand.levels = expand.levels)
  ## out = gaps(grl, start = start, end = end)
  mcols(out) = mcols(gr)[unix,vars, drop = F]
  if (cleannm)
    names(out) = gsub(sep, " ", names(out))
  return(out)
}


#' @name gr.setdiff2
#' @title gr.setdiff that works with multiple by columns
#' @description
#'
#'
#' @return GRanges
#' @rdname gr.setdiff2
#' @author Kevin Hadi
#' @export gr.setdiff2
gr.setdiff2 = function (query, subject, ignore.strand = TRUE, by = NULL, new = TRUE, ...)
{
  if (!is.null(by)) {
    if (ignore.strand) {
      query = gr.stripstrand(query)
      subject = gr.stripstrand(subject)
    }
    sl = seqlengths(query)
    if (new) {
      ## gp = do.call(gr.splgaps, c(alist(gr = gr.fix(subject, query)), ... = lapply(by, str2lang)))
      cmd = sprintf("gr.splgaps(gr.fix(subject, query), %s, expand.levels = TRUE)", paste(collapse = ",", by))
      gp = eval(parse(text = cmd))
    } else {
      tmp = gr2dt(subject)
      tmp$strand = factor(tmp$strand, c("+", "-", "*"))
      gp = dt2gr(tmp[, as.data.frame(gaps(GRanges(seqnames,
        IRanges(start, end), seqlengths = sl, strand = strand))),
      , by = by], seqinfo = seqinfo(query))
    }
    qdt = as.data.table(mcols(gr.noval(query, keep.col = by)))[, query.id := seq_len(.N)]
    sdt = as.data.table(mcols(gr.noval(subject, keep.col = by)))[, subject.id := seq_len(.N)]
    mdt = merge(setkeyv(unique(qdt[,-c("query.id")][, inx := TRUE]), by),
      setkeyv(unique(sdt[, -c("subject.id")][, iny := TRUE]), by), all = T)[is.na(iny)]
    rm(sdt)
    gp = grl.unlist(gp)
    if (nrow(mdt)) {
      gp = grbind(gp, gr.spreduce(gr.noval(query[merge(qdt, mdt, by = by)$query.id],
        keep.col = by), by))
    }
    rm(mdt, qdt)
    if (ignore.strand)
      gp = gr.stripstrand(gp[strand(gp) == "*"])
  }
  else {
    if (ignore.strand) {
      gp = gaps(gr.stripstrand(subject)) %Q% (strand ==
                                                "*")
    }
    else {
      gp = gaps(subject)
    }
  }
  if (new) {
    out = suppressWarnings({gr.findoverlaps(gr_construct_by(query, by), gr_construct_by(gp, by),
      qcol = names(values(query)),
      ignore.strand = ignore.strand, ...)})
    out = gr.fix(gr_deconstruct_by(out, by = by), query)
  } else {
    out = gr.findoverlaps(query, gp,
      qcol = names(values(query)),
      ignore.strand = ignore.strand, by = by, ...)
  }
  return(out)
}


## gr.splgaps = function(gr, ..., sep = paste0(" ", rand.string(length = 8), " "), start = 1L, end = seqlengths(gr), cleannm = TRUE) {
##   lst = as.list(match.call())[-1]
##   ix = which(!names(lst) %in% c("gr", "sep", "cleannm", "start", "end"))
##   tmpix = with(as.list(mcols(gr)), do.call(paste, c(lst[ix], alist(sep = sep))))
##   tmpix = factor(tmpix, levels = unique(tmpix))
##   grl = gr %>% GenomicRanges::split(tmpix)
##   ## out = tmpgrlgaps(grl, start = start, end = end)
##   out = gaps(grl, start = start, end = end)
##   mcols(out) = data.table::tstrsplit(names(out), sep)
##   colnames(mcols(out)) = unlist(strsplit(toString(lst[ix]), ", "))
##   if (cleannm)
##     names(out) = gsub(sep, " ", names(out))
##   out
## }



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

gr.poverlaps = function(gr1, gr2, ignore.strand = FALSE, as.logical = TRUE) {
    minoverlaps <<- 1
    minoverlap <<- 0
    out = IRanges::poverlaps(gr.fix(gr1, gr2), gr.fix(gr2, gr1), ignore.strand = ignore.strand)
    if (as.logical) {
        return(as.logical(out))
    } else {
        return(out)
    }
}

inherits.edf = function(DF) {
    if (all(sapply(DF, class) %in% c("integer","GRanges","CompressedGRangesList","logical", "GRangesList"))) {
        if (inherits(DF, "DataFrame")) {
            if (all(c("from","to","from.gr","to.gr","bp.left","bp.right","junction","ref") %in% colnames(DF))) {
                return(TRUE)
            } else {
                return(FALSE)
            }
        } else {
            return(FALSE)
        }
    } else {
        return(FALSE)
    }
}




#' @name ra.overlaps6
#'
#' One of the many rewrites of ra.overlaps
#'
#' @export
ra.overlaps6 <- function(ra1, ra2, pad = 0, ignore.strand = ignore.strand) {
    ra1 = gr.noval(ra1)
    ra2 = gr.noval(ra2)
    bp1 = grl.unlist(ra1) + pad
    bp2 = grl.unlist(ra2) + pad
    ## data.table::foverlaps
    ix2 = findOverlaps(bp1, bp2, ignore.strand = FALSE)
    ix2 = as.data.table(ix2)
    ix2$grl.ix.x = bp1$grl.ix[ix2$queryHits]
    ix2$grl.iix.x = bp1$grl.iix[ix2$queryHits]
    ix2$grl.ix.y = bp2$grl.ix[ix2$subjectHits]
    ix2$grl.iix.y = bp2$grl.iix[ix2$subjectHits]
    ## ix2 = unname(plyranges::find_overlaps_directed(bp1, bp2))
    ## ix2 = gr2dt(ix2)
    mat = ix2[, cbind(grl.ix.x, grl.ix.y)]
    ## letsee = apply(mat, 1, function(x) c(min(x), max(x)))
    ## ix2[, uix := paste(letsee[1,], letsee[2,])]
    ix2[, uix := paste(matrixStats::rowMins(mat), matrixStats::rowMaxs(mat))]
    ## ix2[, ra.match := all(c(1,2) %in% grl.iix.x) & all(c(1,2) %in% grl.iix.y), by = .(grl.ix.x, grl.ix.y)]
    ok1 = unique(ix2[, .(uix, grl.iix.x)])
    ok1[, has1 := any(grl.iix.x == 1), by = uix]
    ok1[, has2 := any(grl.iix.x == 2), by = uix]
    ok2 = unique(ix2[, .(uix, grl.iix.y)])
    ok2[, has1 := any(grl.iix.y == 1), by = uix]
    ok2[, has2 := any(grl.iix.y == 2), by = uix]
    ## ix2[, ra.match := all(c(1,2) %in% grl.iix.x) & all(c(1,2) %in% grl.iix.y), keyby = uix]
    meth1 = intersect(ok1[(has1 & has2)][!duplicated(uix)]$uix, ok2[(has1 & has2)][!duplicated(uix)]$uix)
    ## setdiff(meth1, ix2[ra.match == TRUE]$uix)
    ## setkey(ix2, grl.ix.x, grl.ix.y)
    ## ix2 = ix2[ra.match == TRUE][!duplicated(data.table(grl.ix.x, grl.ix.y))]
    return(setkey(ix2[!duplicated(uix)], uix)[meth1][, cbind(grl.ix.x, grl.ix.y)])
    ## return(ix2[ra.match == TRUE][!duplicated(data.table(grl.ix.x, grl.ix.y))][, cbind(grl.ix.x, grl.ix.y)])
}

#' @name ra.dedup6
#'
#' One of the many rewrites of ra.overlaps
#'
#' @export
ra.dedup6 <- function (grl, pad = 500, ignore.strand = FALSE) 
{
    if (!is(grl, "GRangesList")) {
        stop("Error: Input must be GRangesList!")
    }
    if (any(elementNROWS(grl) != 2)) {
        stop("Error: Each element must be length 2!")
    }
    if (length(grl) == 0 | length(grl) == 1) {
        return(grl)
    }
    if (length(grl) > 1) {
        ix.pair = as.data.table(ra.overlaps6(grl, grl, pad = pad, 
            ignore.strand = ignore.strand))[grl.ix.x != grl.ix.y]
        if (nrow(ix.pair) == 0) {
            return(grl)
        }
        else {
            dup.ix = unique(rowMax(as.matrix(ix.pair)))
            return(grl[-dup.ix])
        }
    }
}


## ra.overlaps6 <- function(ra1, ra2, pad = 0, ignore.strand = ignore.strand) {
##     ra1 = gr.noval(ra1)
##     ra2 = gr.noval(ra2)
##     bp1 = grl.unlist(ra1) + pad
##     bp2 = grl.unlist(ra2) + pad
##     browser()
##     ## data.table::foverlaps
##     findOverlaps(bp1, bp2, ignore.strand = FALSE)
##     ix2 = unname(plyranges::find_overlaps_directed(bp1, bp2))
##     ix2 = gr2dt(ix2)
##     ix2[, ra.match := all(c(1,2) %in% grl.iix.x & all(c(1,2) %in% grl.iix.y)), by = .(grl.ix.x, grl.ix.y)]
##     ix2 = ix2[ra.match == TRUE][!duplicated(data.table(grl.ix.x, grl.ix.y))]
##     ix2[, cbind(grl.ix.x, grl.ix.y)]
## }




#' @name gr2bed
#'
#' converting gr to bed like table
#' also shifts coordinates to half closed 0 based
#'
#' @export
gr2bed <- function(gr) {
    df = as.data.frame(gr)
    colnames(df)[1:3] = c("chrom", "chromStart", "chromEnd")
    df$width = NULL
    out = cbind(df[,1:3,drop=F], name = as.character(seq_len(NROW(df))),
                score = rep_len(0, NROW(df)), df[,-c(1:3),drop=F])
    out$chromStart = out$chromStart - 1
    return(out)
}


## gr2bed = function(gr) {
##     df = gr2dt(gr) %>% select(-one_of("chr")) %>% rename_at(1:3, ~c("chr", "start", "end")) %>% mutate(chr = as.character(chr), start = start - 1) %>% select(-one_of("width"))
## }


#' @name grl2bedpe
#'
#' converting grl to bedpe-like table
#' also shifts coordinates to half closed 0 based
#'
#' @export
grl2bedpe = function(grl, add_breakend_mcol = FALSE, flip = FALSE, as.data.table = TRUE, zerobased = TRUE) {
    grpiv = grl.pivot(grl)
    if (zerobased) {
        grpiv[[1]] = gr.resize(grpiv[[1]], width = 2, pad = FALSE, fix = "end")
        grpiv[[2]] = gr.resize(grpiv[[2]], width = 2, pad = FALSE, fix = "end")
    }


    mcgrl = as.data.frame(mcols(grl))

    df1 = as.data.frame(grpiv[[1]])[, c(1:3, 5), drop=F]
    df2 = as.data.frame(grpiv[[2]])[, c(1:3, 5), drop=F]
    colnames(df1) = c("chrom1", "start1", "end1", "strand1")
    colnames(df2) = c("chrom2", "start2", "end2", "strand2")

    mc1 = data.frame()[seq_len(NROW(df1)),,drop=F]
    mc2 = data.frame()[seq_len(NROW(df2)),,drop=F]

    if (isTRUE(add_breakend_mcol)) {
        mc1 = as.data.frame(grpiv[[1]])[,-c(1:5),drop=F]
        mc2 = as.data.frame(grpiv[[2]])[,-c(1:5),drop=F]

        colnames(mc1) = paste0("first.", colnames(mc1))
        colnames(mc2) = paste0("second.", colnames(mc2))
    }

    out = cbind(df1, df2, name = as.character(seq_len(NROW(df1))), score = rep_len(0, NROW(df1)), mcgrl, mc1, mc2)

    canon_col = c(1, 2, 3, 5, 6, 7, 9, 10, 4, 8)
    ## out[, canon_col, drop = F]
    nix = seq_len(ncol(out))

    ## reorder
    out = out[, c(canon_col, nix[!nix %in% canon_col]), drop = F]

    if (flip) {
        out$strand1 = c("+" = "-", "-" = "+")[out$strand1]
        out$strand2 = c("+" = "-", "-" = "+")[out$strand2]
    }

    if (as.data.table)
        return(as.data.table(out))
    else
        return(out)
    
}

## grl2bedpe = function(grl) {
    ## df = as.data.frame(S4Vectors::zipdown(grl))
    ## df = select(df, -matches("(first|second)\\.width"), -one_of("names"))
    ## ## df = rename_at(df, vars(matches("(\\.X)")), ~gsub("(\\.X)?", "", .))
    ## colnames(df) = withv(colnames(df), gsub("(\\.X)?", "", x))
    ## df = df[,!duplicated(colnames(df))]
    ## df = df %>% rename_at(vars(matches("(first|second)\\.seqnames")), ~gsub("seqnames", "chr", .)) %>%
    ##     rename_at(vars(matches("^first\\.(chr$|start$|end$|strand$)")), ~gsub("(first\\.)(.*)", "\\21", .)) %>%
    ##     rename_at(vars(matches("^second\\.(chr$|start$|end$|strand$)")), ~gsub("(second\\.)(.*)", "\\22", .))
    ## df = mutate_at(df, vars(matches("start(1|2)")), ~(. - 1))
    ## df = mutate(df, name = "dummy", score = 0)
    ## select(df, chr1, start1, end1, chr2, start2, end2, name, score, strand1, strand2, everything())
## }

#' @name bedpe2grl
#'
#' converting bedpe to grl
#'
#' @export
bedpe2grl = function(bedpe, flip = FALSE, trim = TRUE, genome = NULL, sort = TRUE) {
    if (!NROW(bedpe)) return(GRangesList())
    bedpe$chrom1 = as.character(bedpe$chrom1)
    bedpe$chrom2 = as.character(bedpe$chrom2)
    st1 = bedpe$strand1
    st2 = bedpe$strand2
    if (isTRUE(flip)) {
        st1 = c("+" = "-", "-" = "+")[st1]
        st2 = c("+" = "-", "-" = "+")[st2]
    }
    gr1 = data.frame(seqnames = bedpe$chrom1, start = bedpe$start1,
                     end = bedpe$end1, strand = st1)
    gr1 = makeGRangesFromDataFrame(gr1)
    gr1 = gr.resize(gr1, 1, pad = FALSE, fix = "end")
    gr2 = data.frame(seqnames = bedpe$chrom2, start = bedpe$start2,
                end = bedpe$end2, strand = st2)
    gr2 = makeGRangesFromDataFrame(gr2)
    gr2 = gr.resize(gr2, 1, pad = FALSE, fix = "end")
    d1.cols = intersect(c("name", "score"), colnames(bedpe))
    if (length(d1.cols))
        d1 = tryCatch(bedpe[, d1.cols, drop = F, with = F], error = function(e) bedpe[, d1.cols, drop = F])
    else
        d1 = tryCatch(bedpe[, 0, drop = F, with = F], error = function(e) bedpe[, 0, drop = F])
    ## d1 = bedpe[, c("name", "score"), drop=F]
    d2 = bedpe[, -c(1:10), drop=F]    
    grl = grl.pivot(GRangesList(gr1, gr2))
    mcols(grl) = cbind(d1, d2)
    if (sort) grl = gr.sort(grl)
    return(grl)
}

## bedpe2grl = function(bdpe, genome = NULL) {
##     bdpe$chrom1 = as.character(bdpe$chrom1)
##     bdpe$chrom2 = as.character(bdpe$chrom2)
##     dat = tidyr::pivot_longer(bdpe, cols = c("chrom1", "start1", "end1", "strand1", "chrom2", "start2", "end2", "strand2"), names_to = c(".value", "name"), names_pattern = "([A-Za-z]+)([12]$)")
##     dat = dplyr::mutate_at(dat, vars(matches("^start$")), ~(. + 1))
##     dat = dt2gr(dat)
##     return(grl.pivot(gr.fix(split(dat, dat$name), hg_seqlengths(genome = genome))))
## }

gr.shift = function(gr, shift = 1, ignore.strand = FALSE) {
    if (!ignore.strand) {
        return(GenomicRanges::shift(gr, c("+" = 1, "-" = -1)[as.character(strand(gr))] * shift))
    } else {
        return(GenomicRanges::shift(gr, shift))
    }
}

ra.overlaps2 = function(ra1, ra2, pad = 0, ignore.strand = FALSE) {
    if (length(ra1) == 0 | length(ra2) == 0) {
        return(data.table(query.id = as.integer(NA), subject.id = as.integer(NA)))
    }
    ## forcibly removing all metadata before doing the query... the finagling
    ## of metadata could be problematic especially when they are
    ## not standard S3 classes
    ra1@unlistData@elementMetadata = ra1@unlistData@elementMetadata[,c()]
    ra1@elementMetadata = ra1@elementMetadata[,c()]
    ra2@unlistData@elementMetadata = ra2@unlistData@elementMetadata[,c()]
    ra2@elementMetadata = ra2@elementMetadata[,c()]
    bp1 = grl.unlist(ra1)
    bp2 = grl.unlist(ra2)
    bp1 = gr.fix(bp1, bp2)
    sbp1 = seqinfo(bp1)
    bp2 = gr.fix(bp2, bp1)
    sbp2 = seqinfo(bp2)
    bp1 = sort(sortSeqlevels(bp1), ignore.strand = FALSE) + pad
    bp2 = sort(sortSeqlevels(bp2), ignore.strand = FALSE) + pad
    bp1 = gr2dt(bp1)
    bp2 = gr2dt(bp2)
    bp1[, grl.iix := seq_len(.N), by= grl.ix]
    bp2[, grl.iix := seq_len(.N), by= grl.ix]
    data.table::setorderv(bp1, "grl.ix")
    data.table::setorderv(bp2, "grl.ix")
    bp1 = dt2gr(bp1, seqlengths = seqlengths(sbp1), seqinfo = sbp1)
    bp2 = dt2gr(bp2, seqlengths = seqlengths(sbp2), seqinfo = sbp2)
    ## bp1 = gr.fix(bp1, bp2)
    ## bp2 = gr.fix(bp2, bp1)
    ix1 = findOverlaps(bp1 %Q% (grl.iix == 1),
                       bp2 %Q% (grl.iix == 1),
                       ignore.strand = ignore.strand)
    ix2 = findOverlaps(bp1 %Q% (grl.iix == 2),
                       bp2 %Q% (grl.iix == 2),
                       ignore.strand = ignore.strand)
    ix1 = as.data.table(ix1); data.table::setnames(ix1, c("query.id", "subject.id"))
    ix2 = as.data.table(ix2); data.table::setnames(ix2, c("query.id", "subject.id"))
    data.table::setkeyv(ix1, c("query.id", "subject.id"))
    data.table::setkeyv(ix2, c("query.id", "subject.id"))
    if (nrow(ix1) == 0 | nrow(ix2) == 0) {
        return(data.table(query.id = as.integer(NA), subject.id = as.integer(NA)))
    }
    ## mg = merge(ix1, ix2, by = c("query.id", "subject.id"), allow.cartesian = TRUE)
    mg = merge(ix1, ix2, allow.cartesian = TRUE)
    return(mg)
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

#' @name fit.cnv.sig
#' @title fit battenberg copy number to CN signatures (Nature 2022)
#'
#' 
#' @export fit.cnv.sig
fit.cnv.sig = function(gr.seg, sig.cnv = "~/Dropbox/Isabl/HRD/Steele-cnv-signature-definitions.txt",
         id = NULL) {
    
    features = c('0:homdel:0-100kb', '0:homdel:100kb-1Mb', '0:homdel:>1Mb', '1:LOH:0-100kb', 
                 '1:LOH:100kb-1Mb', '1:LOH:1Mb-10Mb', '1:LOH:10Mb-40Mb', '1:LOH:>40Mb', 
                 '2:LOH:0-100kb', '2:LOH:100kb-1Mb', '2:LOH:1Mb-10Mb', '2:LOH:10Mb-40Mb', '2:LOH:>40Mb', 
                 '3-4:LOH:0-100kb', '3-4:LOH:100kb-1Mb', '3-4:LOH:1Mb-10Mb', '3-4:LOH:10Mb-40Mb', '3-4:LOH:>40Mb', 
                 '5-8:LOH:0-100kb', '5-8:LOH:100kb-1Mb', '5-8:LOH:1Mb-10Mb', '5-8:LOH:10Mb-40Mb', '5-8:LOH:>40Mb', 
                 '9+:LOH:0-100kb', '9+:LOH:100kb-1Mb', '9+:LOH:1Mb-10Mb', '9+:LOH:10Mb-40Mb', '9+:LOH:>40Mb', 
                 '2:het:0-100kb', '2:het:100kb-1Mb', '2:het:1Mb-10Mb', '2:het:10Mb-40Mb', '2:het:>40Mb', 
                 '3-4:het:0-100kb', '3-4:het:100kb-1Mb', '3-4:het:1Mb-10Mb', '3-4:het:10Mb-40Mb', '3-4:het:>40Mb', 
                 '5-8:het:0-100kb', '5-8:het:100kb-1Mb', '5-8:het:1Mb-10Mb', '5-8:het:10Mb-40Mb', '5-8:het:>40Mb', 
                 '9+:het:0-100kb', '9+:het:100kb-1Mb', '9+:het:1Mb-10Mb', '9+:het:10Mb-40Mb', '9+:het:>40Mb')


    if (is.character(gr.seg) && file.exists(gr.seg)) {
        gr.seg = readin(gr.seg, other.txt = c("seg"))
    }

    if (inherits(gr.seg, "data.frame")) {
        gr.seg = df2gr(gr.seg, 2, 3, 4)
    } else if (inherits(gr.seg, "GRanges")) {
        NULL
    } else {
        stop("seg must be a path to a CN segmentation file or a GRanges/data frame")
    }
    

    super_class = c('het', 'LOH', "homdel")
    hom_del_class = c('0-100kb', '100kb-1Mb', '>1Mb')
    # x_labels = c('>40Mb', '10Mb-40Mb', '1Mb-10Mb', '100kb-1Mb', '0-100kb')
    x_labels = c("0-100kb","100kb-1Mb","1Mb-10Mb","10Mb-40Mb",">40Mb")
    CN_classes = c("1","2","3-4","5-8","9+") # different total CN states

    gr.seg$hom_seg = cut(width(gr.seg), c(-1, 100e3, 1e6, Inf), labels = hom_del_class)
    gr.seg$x_seg = cut(width(gr.seg), c(-1, 100e3, 1e6, 10e6, 40e6, Inf), labels = x_labels)
    gr.seg$tot_cn = with(gr.seg, nMaj1_A + nMin1_A)
    gr.seg$minor = gr.seg$nMin1_A
    gr.seg$major = gr.seg$nMaj1_A
    gr.seg$cn_state = cut(gr.seg$tot_cn, c(-1, 1, 2, 4, 8, Inf), labels = CN_classes)
    gr.seg$CN_class = with(gr.seg, case_when(tot_cn == 0 ~ "homdel",
                                             minor == 0 & major > 0 ~ "LOH",
                                             TRUE ~ "het"))

    gr.seg$x_lv = with(gr.seg, paste(cn_state, CN_class, x_seg, sep = ":"))
    gr.seg$hom_lv = with(gr.seg, paste(cn_state, CN_class, hom_seg, sep = ":"))
    gr.seg$is_homdel = with(gr.seg, CN_class == "homdel")
    gr.seg$feature = with(gr.seg, factor(ifelse(is_homdel, hom_lv, x_lv), features))

    ## sig.cnv.path = "~/Dropbox/Isabl/HRD/Steele-Nature-2022-supp-table2.xlsx"
    ## esh = readxl::excel_sheets(sig.cnv.path)
    ## "Pancan sig definitions"

    if (is.character(sig.cnv) && file.exists(sig.cnv)) {
        sig.def = read.table(sig.cnv)
    } else if (inherits(sig.def, c("data.frame", "matrix"))) {
        sig.def = sig.cnv
    } else {
        stop("sig.cnv must be a path to signature definition or a matrix/data.frame")
    }

    nbootFit = 100
    methodFit = "KLD"
    threshold_percentFit = 5
    bootstrapSignatureFit = TRUE
    nbootFit = 100
    threshold_p.valueFit = 0.05
    bootstrapHRDetectScores = FALSE
    nparallel = 1
    randomSeed = 10

    cat_cnv = matrify(as.data.frame(gr.seg$feature %>% table))

    bootstrap_fit_cnv <- signature.tools.lib::SignatureFit_withBootstrap(
                                                  cat_cnv, 
                                                  sig.def, nboot = nbootFit, method = methodFit, 
                                                  threshold_percent = threshold_percentFit, threshold_p.value = threshold_p.valueFit, 
                                                  verbose = FALSE, nparallel = nparallel, randomSeed = randomSeed
                                              )

    exposures_cnv <- bootstrap_fit_cnv$E_median_filtered
    exposures_cnv[is.nan(exposures_cnv)] <- 0

    out = as.data.table(transp(exposures_cnv)[[1]])
    setnames(out, rownames(exposures_cnv))
    if (!is.null(id)) {
        out$id = id
        setcolorder(out, "id")
    }
    return(out)
}

#' @name isv2grl
#' @title isv2grl
#'
#' 
#' @export 
isv2grl = function(sv, flipstrand = TRUE) {
    if (is.character(sv) && file.exists(sv)) {
        sv.path = sv
        vcf = readVcf(sv)
        sv = rowRanges(vcf)
        mcols(sv) = cbind(mcols(sv), info(vcf))
    }
    if (NROW(sv)) {
        gstrands = transp(strsplit(sv$STRANDS, ""), c)
        if (flipstrand) {
            gstrands[[1]] = unname(c("+" = "-", "-" = "+")[gstrands[1][[1]]])
            gstrands[[2]] = unname(c("+" = "-", "-" = "+")[gstrands[2][[1]]])
        }
        strand(sv) = gstrands[[1]]
        sv$ALT = unlist(sv$ALT)
        sv$alt = gsub("[\\[N\\]]", "", sv$ALT, perl = T)
        gr.2 = GRanges(sv$alt)
        strand(gr.2) = gstrands[[2]]
        grl = GRangesList(unname(gr.noval(sv)), unname(gr.2))
        grl = grl.pivot(grl)
    } else {
        grl = GRangesList()
    }
    mcols(grl) = mcols(sv)
    return(grl)
}




##############################
##############################

#' @name parsesnpeff
#' @title parse snpeff output into granges
#'
#'
#' @param vcf path to snpeff vcf
#' @param pad Exposed argument to skitools::ra.overlaps()
#' @return GRangesList of breakpoint pairs with junctions that overlap removed
#' @export
parsesnpeff = function (vcf, id = NULL, filterpass = TRUE, coding_alt_only = TRUE, 
    geno = NULL, gr = NULL, keepfile = FALSE, altpipe = FALSE, 
    debug = FALSE, snpeffpath = "~/modules/SnpEff", filters = "PASS,.") 
{
    if (debug) 
        browser()
    out.name = paste0("tmp_", rand.string(), ".vcf.gz")
    tmp.path = paste0(tempdir(), "/", out.name)
    if (!keepfile) 
        on.exit(unlink(tmp.path))
    try2({
        catcmd = if (grepl("(.gz)$", vcf)) "zcat" else "cat"
        ## onepline = "/gpfs/commons/groups/imielinski_lab/git/mskilab/flows/modules/SnpEff/source/snpEff/scripts/vcfEffOnePerLine.pl"
        onepline = paste0(snpeffpath, "/source/snpEff/scripts/vcfEffOnePerLine.pl")
        if (coding_alt_only) {
            filt = sprintf("java -Xmx20m -Xms20m -XX:ParallelGCThreads=1 -jar %s filter \"( ANN =~ 'chromosome_number_variation|exon_loss_variant|rare_amino_acid|stop_lost|transcript_ablation|coding_sequence|regulatory_region_ablation|TFBS|exon_loss|truncation|start_lost|missense|splice|stop_gained|frame' )\"",
                           paste0(snpeffpath, "/source/snpEff/SnpSift.jar"))
            if (filterpass)
                cmd = sprintf(paste("bcftools view -f %s %s | %s | %s | bgzip -c > %s"), 
                  filters, vcf, onepline, filt, tmp.path)
            else cmd = sprintf("cat %s | %s | %s | bcftools norm -Ov -m-any | bgzip -c > %s", 
                vcf, onepline, filt, tmp.path)
        }
        else {
            filt = ""
            if (filterpass) 
                cmd = sprintf(paste(catcmd, "%s | %s | bcftools view -i 'FILTER==\"PASS\"' | bgzip -c > %s"), 
                  vcf, onepline, tmp.path)
            else cmd = sprintf(paste(catcmd, "%s | %s | bcftools norm -Ov -m-any | bgzip -c > %s"), 
                vcf, onepline, tmp.path)
        }
        system(cmd)
    })
    if (!altpipe) 
        out = grok_vcf(tmp.path, long = TRUE, geno = geno, gr = gr)
    else {
        vcf = readVcf(tmp.path)
        vcf = S4Vectors::expand(vcf)
        rr = within(rowRanges(vcf), {
            REF = as.character(REF)
            ALT = as.character(ALT)
        })
        ann = as.data.table(tstrsplit(unlist(info(vcf)$ANN), 
            "\\|"))[, 1:15, with = FALSE, drop = FALSE]
        fn = c("allele", "annotation", "impact", "gene", "gene_id", 
            "feature_type", "feature_id", "transcript_type", 
            "rank", "variant.c", "variant.p", "cdna_pos", "cds_pos", 
            "protein_pos", "distance")
        data.table::setnames(ann, fn)
        if ("AD" %in% names(geno(vcf))) {
            adep = setnames(as.data.table(geno(vcf)$AD[, , 1:2]), 
                c("ref", "alt"))
            gt = geno(vcf)$GT
        }
        else if (all(c("AU", "GU", "CU", "TU", "TAR", "TIR") %in% 
            c(names(geno(vcf))))) {
            this.col = dim(geno(vcf)[["AU"]])[2]
            d.a = geno(vcf)[["AU"]][, , 1, drop = F][, this.col, 
                1]
            d.g = geno(vcf)[["GU"]][, , 1, drop = F][, this.col, 
                1]
            d.t = geno(vcf)[["TU"]][, , 1, drop = F][, this.col, 
                1]
            d.c = geno(vcf)[["CU"]][, , 1, drop = F][, this.col, 
                1]
            mat = cbind(A = d.a, G = d.g, T = d.t, C = d.c)
            rm("d.a", "d.g", "d.t", "d.c")
            refid = match(as.character(VariantAnnotation::fixed(vcf)$REF), colnames(mat))
            refid = ifelse(!isSNV(vcf), NA_integer_, refid)
            altid = match(as.character(VariantAnnotation::fixed(vcf)$ALT), colnames(mat))
            altid = ifelse(!isSNV(vcf), NA_integer_, altid)
            refsnv = mat[cbind(seq_len(nrow(mat)), refid)]
            altsnv = mat[cbind(seq_len(nrow(mat)), altid)]
            this.icol = dim(geno(vcf)[["TAR"]])[2]
            refindel = d.tar = geno(vcf)[["TAR"]][, , 1, drop = F][, 
                this.icol, 1]
            altindel = d.tir = geno(vcf)[["TIR"]][, , 1, drop = F][, 
                this.icol, 1]
            adep = data.table(ref = coalesce(refsnv, refindel), 
                alt = coalesce(altsnv, altindel))
            gt = NULL
        }
        else {
            message("ref and alt count columns not recognized")
            adep = NULL
            gt = NULL
        }
        mcols(rr) = BiocGenerics::cbind(mcols(rr), ann, adep, 
            gt = gt[, 1])
        out = rr
    }
    this.env = environment()
    return(this.env$out)
}



## parsesnpeff = function (vcf, id = NULL, filterpass = TRUE, coding_alt_only = TRUE, 
##     geno = NULL, gr = NULL, keepfile = FALSE, altpipe = FALSE, 
##     debug = FALSE, snpeffpath = "~/modules/SnpEff") 
## {
##     if (debug) 
##         browser()
##     out.name = paste0("tmp_", rand.string(), ".vcf.gz")
##     tmp.path = paste0(tempdir(), "/", out.name)
##     if (!keepfile) 
##         on.exit(unlink(tmp.path))
##     try2({
##         catcmd = if (grepl("(.gz)$", vcf)) "zcat" else "cat"
##         ## onepline = "/gpfs/commons/groups/imielinski_lab/git/mskilab/flows/modules/SnpEff/source/snpEff/scripts/vcfEffOnePerLine.pl"
##         onepline = paste0(snpeffpath, "/source/snpEff/scripts/vcfEffOnePerLine.pl")
##         if (coding_alt_only) {
##             filt = sprintf("java -Xmx20m -Xms20m -XX:ParallelGCThreads=1 -jar %s filter \"( ANN =~ 'chromosome_number_variation|exon_loss_variant|rare_amino_acid|stop_lost|transcript_ablation|coding_sequence|regulatory_region_ablation|TFBS|exon_loss|truncation|start_lost|missense|splice|stop_gained|frame' )\"",
##                            paste0(snpeffpath, "/source/snpEff/SnpSift.jar"))
##             if (filterpass)
##                 cmd = sprintf(paste("bcftools view -i 'FILTER==\"PASS\"' %s | %s | %s | bgzip -c > %s"), 
##                   vcf, onepline, filt, tmp.path)
##             else cmd = sprintf("cat %s | %s | %s | bcftools norm -Ov -m-any | bgzip -c > %s", 
##                 vcf, onepline, filt, tmp.path)
##         }
##         else {
##             filt = ""
##             if (filterpass) 
##                 cmd = sprintf(paste(catcmd, "%s | %s | bcftools view -i 'FILTER==\"PASS\"' | bgzip -c > %s"), 
##                   vcf, onepline, tmp.path)
##             else cmd = sprintf(paste(catcmd, "%s | %s | bcftools norm -Ov -m-any | bgzip -c > %s"), 
##                 vcf, onepline, tmp.path)
##         }
##         system(cmd)
##     })
##     if (!altpipe) 
##         out = grok_vcf(tmp.path, long = TRUE, geno = geno, gr = gr)
##     else {
##         vcf = readVcf(tmp.path)
##         vcf = S4Vectors::expand(vcf)
##         rr = within(rowRanges(vcf), {
##             REF = as.character(REF)
##             ALT = as.character(ALT)
##         })
##         ann = as.data.table(tstrsplit(unlist(info(vcf)$ANN), 
##             "\\|"))[, 1:15, with = FALSE, drop = FALSE]
##         fn = c("allele", "annotation", "impact", "gene", "gene_id", 
##             "feature_type", "feature_id", "transcript_type", 
##             "rank", "variant.c", "variant.p", "cdna_pos", "cds_pos", 
##             "protein_pos", "distance")
##         data.table::setnames(ann, fn)
##         if ("AD" %in% names(geno(vcf))) {
##             adep = setnames(as.data.table(geno(vcf)$AD[, , 1:2]), 
##                 c("ref", "alt"))
##             gt = geno(vcf)$GT
##         }
##         else if (all(c("AU", "GU", "CU", "TU", "TAR", "TIR") %in% 
##             c(names(geno(vcf))))) {
##             this.col = dim(geno(vcf)[["AU"]])[2]
##             d.a = geno(vcf)[["AU"]][, , 1, drop = F][, this.col, 
##                 1]
##             d.g = geno(vcf)[["GU"]][, , 1, drop = F][, this.col, 
##                 1]
##             d.t = geno(vcf)[["TU"]][, , 1, drop = F][, this.col, 
##                 1]
##             d.c = geno(vcf)[["CU"]][, , 1, drop = F][, this.col, 
##                 1]
##             mat = cbind(A = d.a, G = d.g, T = d.t, C = d.c)
##             rm("d.a", "d.g", "d.t", "d.c")
##             refid = match(as.character(VariantAnnotation::fixed(vcf)$REF), colnames(mat))
##             refid = ifelse(!isSNV(vcf), NA_integer_, refid)
##             altid = match(as.character(VariantAnnotation::fixed(vcf)$ALT), colnames(mat))
##             altid = ifelse(!isSNV(vcf), NA_integer_, altid)
##             refsnv = mat[cbind(seq_len(nrow(mat)), refid)]
##             altsnv = mat[cbind(seq_len(nrow(mat)), altid)]
##             this.icol = dim(geno(vcf)[["TAR"]])[2]
##             refindel = d.tar = geno(vcf)[["TAR"]][, , 1, drop = F][, 
##                 this.icol, 1]
##             altindel = d.tir = geno(vcf)[["TIR"]][, , 1, drop = F][, 
##                 this.icol, 1]
##             adep = data.table(ref = coalesce(refsnv, refindel), 
##                 alt = coalesce(altsnv, altindel))
##             gt = NULL
##         }
##         else {
##             message("ref and alt count columns not recognized")
##             adep = NULL
##             gt = NULL
##         }
##         mcols(rr) = BiocGenerics::cbind(mcols(rr), ann, adep, 
##             gt = gt[, 1])
##         out = rr
##     }
##     this.env = environment()
##     return(this.env$out)
## }

#' @name grok_vcf
#' @title modded grok_vcf
#'
#'
#' @param x path to vcf
#' @return GRanges
#' @author Marcin Imielinski
#' @export
grok_vcf <- function(x, label = NA, keep.modifier = TRUE, long = FALSE, oneliner = FALSE, verbose = FALSE, geno = NULL, tmp.dir = tempdir(), gr = NULL)
{
  fn = c('allele', 'annotation', 'impact', 'gene', 'gene_id', 'feature_type', 'feature_id', 'transcript_type', 'rank', 'variant.c', 'variant.p', 'cdna_pos', 'cds_pos', 'protein_pos', 'distance')

  if (is.character(x))
    {
        out = suppressWarnings(read_vcf(x, tmp.dir = tmp.dir, geno = geno, gr = gr))
        if (length(out) == 0) {
            return(out)
        }
      if (is.na(label))
        label = x
    }
  else
    out = x

  if (is.na(label))
    label = ''

  if (verbose)
    message('Grokking vcf ', label)

  if (!long)
  {
        vcf = out
        if (length(vcf)>0)
        {
          if (!is.null(vcf$ANN))
          {
            vcf$eff = unstrsplit(vcf$ANN)
            vcf$modifier = !grepl('(HIGH)|(LOW)|(MODERATE)', vcf$eff)
            if (!keep.modifier)
              vcf = vcf[!vcf$modifier]
          }
          vcf$ref = as.character(vcf$REF)
          vcf$alt = as.character(unstrsplit(vcf$ALT))
          vcf = vcf[, sapply(values(vcf), class) %in% c('factor', 'numeric', 'integer', 'logical', 'character')]
          vcf$var.id = 1:length(vcf)
          vcf$type = ifelse(nchar(vcf$ref)==nchar(vcf$alt), 'SNV',
                     ifelse(nchar(vcf$ref)<nchar(vcf$alt),
                            'INS', 'DEL'))
          vcf$label = label
        }
        return(vcf)
  }
  else if (length(out)>0)
    {
        out$REF = as.character(out$REF)
        out$ALT = as.character(unstrsplit(out$ALT))
        out$vartype = ifelse(nchar(out$REF) == nchar(out$ALT), 'SNV',
                      ifelse(nchar(out$REF) < nchar(out$ALT), 'INS', 'DEL'))
        tmp = lapply(out$ANN, function(y) do.call(rbind, lapply(strsplit(y, '\\|'), '[', 1:15)))
        tmpix = rep(1:length(out), sapply(tmp, NROW))
        meta = as.data.frame(do.call(rbind, tmp))
        colnames(meta) = fn
        meta$varid = tmpix
        meta$file = x
        out2 = out[tmpix]
        rownames(meta) = NULL
        values(out2) = cbind(values(out2), meta)
        names(out2) = NULL
        out2$ANN = NULL
        if (oneliner)
          out2$oneliner = paste(
            ifelse(!is.na(out2$gene),
                   as.character(out2$gene),
                   as.character(out2$annotation)),
            ifelse(nchar(as.character(out2$variant.p))>0,
                   as.character(out2$variant.p),
                   as.character(out2$variant.c)))
    }
    return(out2)
}

#' @name est_snv_cn_stub
#' @title estimate snv cn stub
#'
#'
#' @param vcf path to vcf
#' @param jab path to jabba
#' @export
est_snv_cn_stub = function (vcf, jab, tumbam = NULL, germ_subsample = 200000, somatic = FALSE, 
    saveme = FALSE) 
{
    oldsaf = options()$stringsAsFactors
    options(stringsAsFactors = FALSE)
    oldscipen = options()$scipen
    options(scipen = 999)
    on.exit({
        options(scipen = oldscipen)
        options(stringsAsFactors = oldsaf)
        unlink(tmpvcf)
        unlink(tmpvcf2)
    })
    tmpvcf = tempfile(fileext = ".vcf")
    tmpvcf2 = tempfile(fileext = ".vcf")
    if (!somatic) {
        message("starting germline processing")
        system2("bcftools", c("view -i 'FILTER==\"PASS\"'", vcf), 
            stdout = tmpvcf)
        system2("java", sprintf("-jar ~/software/jvarkit/dist/downsamplevcf.jar -N 10 -n %s %s", 
            germ_subsample, tmpvcf), stdout = tmpvcf2, env = "module unload java; module load java/1.8;")
        gvcf = parsesnpeff(tmpvcf, coding_alt_only = TRUE, keepfile = FALSE, 
            altpipe = TRUE)
        gvcf_subsam = parsesnpeff(tmpvcf2, coding_alt_only = FALSE, 
            keepfile = FALSE, altpipe = TRUE)
        gvcf = unique(dt2gr(rbind(gr2dt(gvcf_subsam), gr2dt(gvcf))))
        input = gvcf
        rm("gvcf", "gvcf_subsam")
        fif = file.info(dir("./"))
        fif = arrange(cbind(path = rownames(fif), fif), desc(mtime))
        tmp.t = grep("reg_.*.tsv", fif$path, value = TRUE)[1]
        tmp.b = grep("reg_.*.bed", fif$path, value = TRUE)[1]
        callout = grep("mpileup_", fif$path, value = TRUE)[1]
        if (!file.exists(tmp.t)) 
            tmp.t = tempfile(pattern = "reg_", fileext = ".tsv", 
                tmpdir = ".")
        if (!file.exists(tmp.b)) 
            tmp.b = tempfile(pattern = "reg_", fileext = ".bed", 
                tmpdir = ".")
        if (!file.exists(callout)) 
            callout = tempfile(pattern = "mpileup_", fileext = ".vcf", 
                tmpdir = ".")
        input = within(input, {
            nref = nchar(REF)
            nalt = nchar(ALT)
            vartype = ifelse(nref > 1 & nalt == 1, "DEL", ifelse(nref == 
                1 & nalt > 1, "INS", ifelse(nref == 1 & nalt == 
                1, "SNV", NA_character_)))
            maxchar = pmax(nref, nalt)
        })
        input$nref = NULL
        input$nalt = NULL
        input2 = GenomicRanges::reduce(gr.resize(input, ifelse(input$maxchar > 
            1, 201, input$maxchar), pad = FALSE) %>% gr.sort)
        fwrite(gr2dt(input2[, c()])[, 1:3, with = F][, `:=`(start, 
            pmax(start, 1))][, `:=`(end, pmax(end, 1))], tmp.t, 
            sep = "\t", col.names = FALSE)
        fwrite(gr2dt(input2[, c()])[, 1:3, with = F][, `:=`(start, 
            pmax(start - 1, 0))][, `:=`(end, pmax(end, 1))], 
            tmp.b, sep = "\t", col.names = FALSE)
        if (!file.exists(callout)) {
            message("starting germline mpileup to call variants in tumor")
            clock(system(sprintf("(bcftools mpileup -d 8000 -Q 0 -q 0 -B -R %s -f ~/DB/GATK/human_g1k_v37_decoy.fasta %s | bcftools call -m --prior 0 -v) > %s", 
                tmp.t, tumbam, callout)))
        }
        excls = tempfile(pattern = "excludesam_", fileext = ".txt", 
            tmpdir = tempdir())
        writeLines(system(sprintf("bcftools query -l %s", callout), 
            intern = T), excls)
        cntmp = tempfile(pattern = "cntmp_", fileext = ".vcf.gz", 
            tmpdir = "./")
        system(sprintf("(bcftools view -S ^%s %s | bcftools norm -c f -f /gpfs/commons/home/khadi/DB/GATK/human_g1k_v37_decoy.fasta | bcftools norm -Ov -m-any | bgzip -c) > %s", 
            excls, callout, cntmp))
        cnfin = S4Vectors::expand(readVcf(cntmp))
        dp4mat = do.call(rbind, as.list(info(cnfin)$DP4))
        altv = dp4mat[, 3] + dp4mat[, 4]
        idv = info(cnfin)$IDV
        refv = dp4mat[, 1] + dp4mat[, 2]
        idrefv = (altv + refv) - idv
        gr4est = rowRanges(cnfin)
        gr4est$ref = coalesce(idrefv, refv)
        gr4est$alt = coalesce(idv, altv)
        gr4est$ALT = as.character(gr4est$ALT)
        gr4est$REF = as.character(gr4est$REF)
        gr4est = merge(gr2dt(input), gr2dt(gr4est)[, `:=`(pileupfound, 
            TRUE)], by = c("seqnames", "start", "end", "REF", 
            "ALT"), suffixes = c("_normal", ""), all = TRUE, 
            allow.cartesian = TRUE)
        gr4est = unique(gr4est)
        gr4est[, `:=`(pileupfound, pileupfound %in% TRUE)]
        if (saveme) 
            saveRDS(gr4est, "gr4est_germline.rds")
        hold = gr4est[is.na(ref)]
        hold[, `:=`(pileupnotfound, TRUE)]
        germbin = gr4est[is.na(ref_normal)]
        if (saveme) 
            saveRDS(germbin, "germpileupbin.rds")
        gr4est = gr4est[!is.na(ref)][!is.na(ref_normal)]
    }
    else {
        message("reading in somatic variants")
        gr4est = parsesnpeff(vcf, coding_alt_only = FALSE, keepfile = FALSE, 
            altpipe = TRUE)
        if (saveme) 
            saveRDS(gr4est, "gr4est_somatic.rds")
        hold = NULL
    }
    out = est_snv_cn(gr4est, jab, somatic = somatic)
    out = rbind(out, hold, fill = TRUE)
    if (saveme) 
        if (somatic) 
            saveRDS(out, "est_snv_cn_somatic.rds")
        else saveRDS(out, "est_snv_cn_germline.rds")
    return(out)
}

#' @name est_snv_cn
#' @title estimate snv cn
#'
#'
#' @param gr GRanges
#' @param jab jabba rds or jabba list object
#' @export
est_snv_cn = function(gr, jabba, somatic = FALSE) {
  if (length(gr) == 0)
    return(NULL)
  if (is.character(jabba))
    jab = readRDS(jabba)
  gg = gG(jabba = jab)
  lpp = with(jab, list(purity = purity, ploidy = ploidy))
  if (inherits(gr, "data.table"))
    gr = dt2gr(gr)
  gr = gr %*% within(gg$nodes$gr[, c("snode.id", "cn")], {segwid = width})
  dt = gr2dt(gr)
  dt = dt[order(seqnames, start, end, -alt)][, rtot := ref + alt]
  dt = cbind(dt, with(dt, as.data.table(rleseq(seqnames, start, REF, ALT, clump = T))))
  dt[, rtot := sum(ref[1], alt[1]), by = idx]
  dt[, seg_rtot := {u = !duplicated(idx); mean(rtot[u]) %>% round}, by = snode.id]
  dt[, vaf := alt / rtot]
  dt[, vaf_segt := pinch(alt / seg_rtot, 0, 1)]
  if (isFALSE(somatic)) {
    message("calculating cn of somatic variants")
    dt[, norm_term := ifelse(grepl("^0[/|]1$", gt), 2, ifelse(grepl("^1[/|]1$", gt), 1, NA_integer_))]
    dt[!is.na(cn),
       c("est_cn", "est_cn_rm", "est_cn_ll", "est_cn_llrm") :=
         {
           cn = cn[1]
           rtot = rtot[1]
           seg_rtot = seg_rtot[1]
           vaf_segt = vaf_segt[1]
           alt = alt[1]
           norm_term = norm_term[1]
           estcn = round((cn * ((norm_term * vaf) - ((1 - lpp$purity)))) / lpp$purity)
           estcnrm = round((cn * ((norm_term * vaf_segt) - ((1 - lpp$purity)))) / lpp$purity)
           centers = pinch((lpp$purity * (0:cn) / cn ) + ((1 - lpp$purity) / 2))
           ifun = function(cnid, rtot, vaf, alt) {
             dbinom(alt, rtot, prob = centers[cnid + 1], log = T)
           }
           estllcn = which.max(
               withv(sapply((0:cn), ifun,
                            rtot = rtot, vaf = vaf, alt = alt), x - min(x))) - 1
           estllrcn = which.max(
               withv(sapply((0:cn), ifun,
                            rtot = seg_rtot, vaf = vaf_segt, alt = alt), x - min(x))) - 1
           list(estcn,
                estcnrm,
                estllcn,
                estllrcn)
         }, by = .(snode.id, idx)]
  } else {
    message("calculating cn of normal variants")
    dt[!is.na(cn),
       c("est_cn", "est_cn_rm", "est_cn_ll", "est_cn_llrm") :=
         {
           cn = cn[1]
           rtot = rtot[1]
           seg_rtot = seg_rtot[1]
           vaf_segt = vaf_segt[1]
           alt = alt[1]
           estcn = round((cn * (2 * vaf)) / lpp$purity)
           estcnrm = round((cn * (2 * vaf_segt)) / lpp$purity)
           centers = pinch((lpp$purity * (0:cn) / cn ))
           ifun = function(cnid, rtot, vaf, alt) {
             out = dbinom(alt, rtot, prob = centers[cnid + 1], log = T)
             out = replace(out, is.infinite(out) & sign(out) < 0,  -2e9)
             out = replace(out, is.infinite(out) & sign(out) > 0,   2e9)
             return(out)
           }
           estllcn =
             which.max(
               withv(sapply((0:cn), ifun,
                            rtot = rtot, vaf = vaf, alt = alt), x - min(x))) - 1
           estllrcn = which.max(
               withv(sapply((0:cn), ifun,
                            rtot = seg_rtot, vaf = vaf_segt, alt = alt), x - min(x))) - 1
           list(estcn,
                estcnrm,
                estllcn,
                estllrcn)
         }, by = .(snode.id, idx)]
  }
  return(dt)
}

#' @name behomology
#' @title breakend exact homology
#'
#' @description
#' pull out the
#'
#' @param gg gGraph (R6) object
#' @param hg character path to fasta or rtracklayer representation of fasta-like file
#' @export
behomology = function (gg, hg, PAD = 2) {
    if (inherits(gg, "gGraph")) {
        gg = khtools::copy2(gg)
        ed = gg$edges[type == "ALT"]
        if (!length(ed)) {
            gg$edges$mark(bh = NA_integer_)
            gg$edges$mark(bh.1 = NA_integer_)
            return(gg)
        }
        ## bp1 = ed$junctions$left %>% gr.flipstrand
        bp1 = ed$junctions$left %>% gr.noval
        bp2 = ed$junctions$right %>% gr.noval
    }
    else if (inherits(gg, "Junction")) {
        if (!length(gg))
            return(gg)
        ## bp1 = gg$left %>% gr.flipstrand
        bp1 = gg$left %>% gr.noval
        bp2 = gg$right %>% gr.noval
    }
    else stop("Input must be either gGraph or Junction object")
    if (is.character(hg))
        hg = khtools::readinfasta(hg)
    bp1 = dt2gr(gr2dt(bp1))
    bp2 = dt2gr(gr2dt(bp2))
    if (length(setdiff(c(seqnames(bp1), seqnames(bp1)), seqlevels(hg))))
        stop("seqnames in breakpoints missing from the provided reference, plesae check and fix the seqlevels of the provided graph / junctions / and/or reference")
    dodo.call = function(FUN, args) {
        if (!is.character(FUN))
            FUN = substitute(FUN)
        cmd = paste(FUN, "(", paste("args[[", 1:length(args),
            "]]", collapse = ","), ")", sep = "")
        return(eval(parse(text = cmd)))
    }
    .getseq = function(hg, gr) {
        res = dodo.call("c", mapply(function(c, s, e) subseq(hg[c],
            start = s, end = e), seqnames(gr) %>% as.character,
            start(gr), end(gr)))
        res = ifelse(strand(gr) == "+", res, reverseComplement(res)) %>%
            DNAStringSet
        return(res)
    }

    collect_seq = function(bp1, bp2, hg, PAD = 50, shift_pos_bp = FALSE) {
        suppressWarnings({
            .getseq = function(hg, gr) {
                res = dodo.call("c", mapply(function(c, s, e) subseq(hg[c],
                                                                     start = s, end = e), seqnames(gr) %>% as.character,
                                            start(gr), end(gr)))
                res = ifelse(strand(gr) == "+", res, reverseComplement(res)) %>%
                    DNAStringSet
                return(res)
            }
            if (isTRUE(shift_pos_bp)) {
                bp1 = shift_right(bp1, ifelse(as.logical(strand(bp1) == "+"), 1, 0))
                bp2 = shift_right(bp2, ifelse(as.logical(strand(bp2) == "+"), 1, 0))
            }
            bpfrag1.l = gr.resize(rep_each(bp1, PAD), 1:PAD, F, fix = "start", ignore.strand = F)
            bpfrag2.l = gr.flipstrand(gr.resize(rep_each(bp2, PAD), 1:PAD, pad = F, fix = "end", ignore.strand = F))
            bpfrag1.r = gr.resize(rep_each(bp1, PAD), 1:PAD, pad = F, fix = "end", ignore.strand = F)
            bpfrag2.r = gr.flipstrand(gr.resize(rep_each(bp2, PAD), 1:PAD, pad = F, fix = "start", ignore.strand = F))
            bpfrag1.fu = gr.resize(rep_each(bp1, PAD), 1:PAD, pad = F, fix = "start", ignore.strand = F)
            bpfrag2.fu = gr.flipstrand(gr.resize(rep_each(bp2, PAD), 1:PAD, pad = F, fix = "start", ignore.strand = F))
            bpfrag1.c = gr.resize(rep_each(bp1, PAD), 1:PAD, pad = F, fix = "center", ignore.strand = F)
            bpfrag2.c = gr.flipstrand(gr.resize(rep_each(bp2, PAD), 1:PAD, pad = F, fix = "center", ignore.strand = F))
            ## exseq1.l = .getseq(hg, bpfrag1.l)
            exseq1.l = tryCatch(hg[bpfrag1.l], error = function(e) .getseq(hg, bpfrag1.l))
            ## exseq2.l = .getseq(hg, bpfrag2.l)
            exseq2.l = tryCatch(hg[bpfrag2.l], error = function(e) .getseq(hg, bpfrag2.l))
            ## exseq1.r = .getseq(hg, bpfrag1.r)
            exseq1.r = tryCatch(hg[bpfrag1.r], error = function(e) .getseq(hg, bpfrag1.r))
            ## exseq2.r = .getseq(hg, bpfrag2.r)
            exseq2.r = tryCatch(hg[bpfrag2.r], error = function(e) .getseq(hg, bpfrag2.r))
            ## exseq1.fu = .getseq(hg, bpfrag1.fu)
            exseq1.fu = tryCatch(hg[bpfrag1.fu], error = function(e) .getseq(hg, bpfrag1.fu))
            ## exseq2.fu = .getseq(hg, bpfrag2.fu)
            exseq2.fu = tryCatch(hg[bpfrag2.fu], error = function(e) .getseq(hg, bpfrag2.fu))
            exseq1.c = tryCatch(hg[bpfrag1.c], error = function(e) .getseq(hg, bpfrag1.c))
            exseq2.c = tryCatch(hg[bpfrag2.c], error = function(e) .getseq(hg, bpfrag2.c))
            dt = data.table(ix = rep(seq_along(bp1), each = PAD),
                       PAD = as.numeric(rep(1:PAD, PAD)),
                       lmatch = exseq1.l == exseq2.l,
                       rmatch = exseq1.r == exseq2.r,
                       fumatch = exseq1.fu == exseq2.fu,
                       cmatch = exseq1.c == exseq2.c)
            dt2 = dt[,
                     .(bh.l = pmax(max(PAD[lmatch]), 0),
                       bh.r = pmax(max(PAD[rmatch]), 0),
                       bh.fu = pmax(max(PAD[fumatch]), 0),
                       bh.c = pmax(max(PAD[cmatch]), 0)), by = ix]
            return(dt2)

        })
    }
    bhom = collect_seq(bp1, bp2, hg, PAD = PAD)
    bhom.1 = collect_seq(bp1, bp2, hg, PAD = PAD, shift_pos_bp = TRUE)

    if (inherits(gg, "gGraph")) {
        et(sprintf("ed$mark(bh%s.fu = bhom$bh.fu)", PAD))
        et(sprintf("ed$mark(bh%s.l = bhom$bh.l)", PAD))
        et(sprintf("ed$mark(bh%s.r = bhom$bh.r)", PAD))
        et(sprintf("ed$mark(bh%s.c = bhom$bh.c)", PAD))
        et(sprintf("ed$mark(bh%s.1fu = bhom.1$bh.fu)", PAD))
        et(sprintf("ed$mark(bh%s.1l = bhom.1$bh.l)", PAD))
        et(sprintf("ed$mark(bh%s.1r = bhom.1$bh.r)", PAD))
        et(sprintf("ed$mark(bh%s.1c = bhom.1$bh.c)", PAD))
    }
    else {
        et(sprintf("gg$mark(bh%s.fu = bhom$bh.fu)", PAD))
        et(sprintf("gg$mark(bh%s.l = bhom$bh.l)", PAD))
        et(sprintf("gg$mark(bh%s.r = bhom$bh.r)", PAD))
        et(sprintf("gg$mark(bh%s.1fu = bhom.1$bh.fu)", PAD))
        et(sprintf("gg$mark(bh%s.c = bhom$bh.c)", PAD))
        et(sprintf("gg$mark(bh%s.1l = bhom.1$bh.l)", PAD))
        et(sprintf("gg$mark(bh%s.1r = bhom.1$bh.r)", PAD))
        et(sprintf("gg$mark(bh%s.1c = bhom.1$bh.c)", PAD))
    }
    return(gg)

}



##############################
##############################


############################## gGnome helpers


#' @name grl.flip
#' @title flip each GRangesList element
#'
#' @description
#' 
#' 
#' @export grl.flip
grl.flip = function(x, flipstrand = TRUE) {
    if (!inherits(x, "GRangesList")) stop("x is not a GRangesList")
    ir = IRanges(start = 1, end = lengths(x))
    ## seems to be much faster than applying revElements directly to GRangesList
    irl = S4Vectors::revElements(as(ir, "IntegerList"))
    out = x[irl]
    if (flipstrand)
        out = gr.flipstrand(out)
    return(out)
}

#' @name alt_in_cis
#' @title grab edges from walk
#'
#' @description
#' get breakpoints
#' 
#' @export
alt_in_cis = function(gw, full = FALSE) {
    edt = copy(gw$edgesdt)
    gwe = gw$edges[as.character(edt$sedge.id)]
    gw.edge.meta = gwe$dt
    edt$type = gw.edge.meta$type
    edt$class = gw.edge.meta$class
    edt = cbind(edt, asdt(with(edt, rleseq(walk.id, type == "ALT", clump = F, use.data.table = F))))
    gr.bp = grl.unlist(sort(gr.noval(gwe$grl), ignore.strand = T))
    grs = gr.string(gr.bp)
    edt$bp1 = grs[gr.bp$grl.iix == 1]
    edt$bp2 = grs[gr.bp$grl.iix == 2]
    ## parasn(edt, asdt(with(edt, rleseq(walk.id, type == "ALT", clump = F, use.data.table = F))), use.data.table = T)
    edt[, alt_adjacent := any(type == "ALT" & lns > 1), by = walk.id]
    if (full)
        return(withAutoprint(edt, echo = FALSE)$value)
    else
        return(unique(edt[, .(walk.id = walk.id, alt_adjacent = alt_adjacent)]))
}


#' @name gw_edges
#' @title pull edge metadata from gwalk
#'
#' @description
#' 
#' @export
gw_edges = alt_in_cis

#' @name getbp
#' @title grab breakpoints from gGraph alt edges and mark with ecluster filters
#'
#' @description
#' get breakpoints
#' 
#' @export
getbp = function(gg, ignore.small = T, ignore.isolated = T, max.small = 1e4, min.isolated = max.small, sort = FALSE) {
    self = gg
    self$edges$mark(ecluster = as.integer(NA))
    altedges = self$edges[type == "ALT", ]
    o.altedges = copy3(altedges)
    if (length(altedges) == 0) {
        return(NULL)
    }
    if (ignore.small) {
        altedges = altedges[!((class == "DUP-like" | class == "DEL-like") & altedges$span <= max.small)]
    }
    deldup = if (length(altedges) == 0) {
        copy3(altedges)[class %in% c("DUP-like", "DEL-like")]
    }
    if (NROW(deldup) > 0 && ignore.isolated) {
        altes = deldup$shadow
        ## altes$sedge.id = altedges[class %in% c("DUP-like", "DEL-like")]$dt[altes$id]$sedge.id
        bp = gr.noval(grl.unlist(altedges$grl), keep.col = c("grl.ix", "grl.iix", "class", "sedge.id"))
        bp$sedge.id.y = bp$sedge.id; bp$sedge.id = NULL
        addon = deldup$dt[altes$id][, .(sedge.id, class)]
        altes$sedge.id = addon$sedge.id
        altes$class = addon$class
        altes$nbp = altes %N% bp # number of breakpoints of any SV that fall within segment
        numsum = altedges$shadow %>% gr.sum # using the shadows of all of the SVs not just dels and dups
        altes = altes %$% numsum
        iso = ((altes) %Q% (score == 1.0))$id
        ## rm.edges = unique(altes[iso] %Q% (width < thresh))$sedge.id ## old
        rm.edges = unique(altes[iso] %Q% (width < min.isolated))$sedge.id
        rm.dups = S4Vectors::with(altes, sedge.id[class == "DUP-like" & nbp <= 2])
        rm.dups = c(rm.dups, dedup.cols(gr2dt(altes %*% bp))[sedge.id != sedge.id.y][class == "DUP-like"][, .(all(1:2 %in% grl.iix), class.1 = class.1[1]), by = .(sedge.id, sedge.id.y)][, all(V1 == TRUE) & all(class.1 == "DUP-like"), by = sedge.id][V1 == TRUE]$sedge.id) # removing dups that have only other nested dups 
        rm.edges = union(rm.edges, rm.dups)
        keepeid = setdiff(altedges$dt$sedge.id, rm.edges)
        altedges = altedges[as.character(keepeid)]
    } # ignoring isolated dup and del edges that are smaller than threshold
    ## if (length(altedges) == 0) {
    ##         message("No junction in this graph")
    ##     }
    ##     return(NULL)
    ## }
    if (NROW(altedges) > 0)
        o.altedges[as.character(altedges$dt$sedge.id)]$mark(ecluster_filter = "pass")
    else
        return(NULL)
    bp = grl.unlist(o.altedges$grl)[, c("grl.ix", "grl.iix", "edge.id", "ecluster_filter")]
    if (isTRUE(sort)) {
        bp = gr.sort(bp, ignore.strand = T)
    }
    bp$m.ix = seq_along(bp)
    bp.dt = gr2dt(bp)
    return(bp.dt)
}

#' @name interbp_dist
#' @title interbp_dist
#'
#' @description
#' interbp_dist
#' 
#' @export
interbp_dist = function(gg) {
    
    bp.dt = getbp(gg, ignore.small = T, ignore.isolated = T)
    if (nodim(bp.dt))
        return(list(bp.dt = NULL, dists = NULL))
    bp.dt = bp.dt[(GenomicRanges::order(gr.stripstrand(dt2gr(bp.dt))))]
    bp.dt[, genome_order := seq_len(.N)]

    distmat = gr.dist(dt2gr(bp.dt), ignore.strand = T)

    dists = asdt(melt(distmat, value.name = "distance"))[Var1 != Var2][!is.na(distance)][order(distance)]
    dists[, c("minidx", "maxidx") := {mat = cbind(Var1, Var2); list(rowMins(mat), rowMaxs(mat))}]

    dists[, grl.ix1 := bp.dt[dists$Var1]$grl.ix]
    dists[, grl.ix2 := bp.dt[dists$Var2]$grl.ix]


    dists[, edge.id1 := bp.dt[dists$Var1]$edge.id]
    dists[, edge.id2 := bp.dt[dists$Var2]$edge.id]


    dists[, grl.iix1 := bp.dt[dists$Var1]$grl.iix]
    dists[, grl.iix2 := bp.dt[dists$Var2]$grl.iix]

    dists[, strand1 := bp.dt[dists$Var1]$strand]
    dists[, strand2 := bp.dt[dists$Var2]$strand]

    dists[, filt1 := bp.dt[dists$Var1]$ecluster_filter]
    dists[, filt2 := bp.dt[dists$Var2]$ecluster_filter]


    dists[, filt1 := bp.dt[dists$Var1]$ecluster_filter]
    dists[, filt2 := bp.dt[dists$Var2]$ecluster_filter]

    dists2 = dists[order(distance)][!duplicated(Var1)][!duplicated(Var2)] ## get every breakend to its nearest neighbor, 2nd dedup means that there is no extramarital partner 
    dists2[, numocc := .N, by = .(minidx, maxidx)]

    dists2[numocc == 2][!duped(minidx, maxidx)][grl.ix1 != grl.ix2]

    goods = dists2[numocc == 2][!duped(minidx, maxidx)][filt1 == "pass" & filt2 == "pass"][grl.ix1 != grl.ix2]

    dists = merge.repl(dists, goods[, .(minidx, maxidx, goodpair = "pass")], by = c("minidx", "maxidx"))[order(distance)] # goodpair = closest pairing

    return(list(bp.dt = bp.dt, dists = dists))
}

#' @name grab.hrdetect.features
#' @title grab hrdetect features from hrdetect results
#' 
#' @description
#' read in HRDetect results
#' 
#' @export grab.hrdetect.features
grab.hrdetect.features <- function(hrdetect_results, goodpairs, field, id.field = id.field) {
    path = hrdetect_results
    res = readRDS(path)
    id = which(goodpairs[[field]] %in% path)
    x = goodpairs[id]
    pr = unique(x[[id.field]])
    df = as.data.table(res$data_matrix)
    df$pair = pr
    cid = which(colnames(df) %in% "pair")
    indels.class = as.data.table(res$indels_classification)
    indels.class$sample = NULL
    hrd_out = as.data.table(res$hrdetect_output)
    cnames = colnames(hrd_out)
    cnames = paste0("w_", cnames)
    cnames[8] = "HRDetect"
    colnames(hrd_out) = cnames
    df = cbind(qmat(df,,cid), qmat(df,,-cid), indels.class, hrd_out)
    return(df)
}

#' @name pairs.grab.hrdetect.features
#' @title grab hrdetect features from pairs table
#' 
#' @description
#' wrapper around grab.hrdetect.features
#' 
#' @export pairs.grab.hrdetect.features
pairs.grab.hrdetect.features <- function(pairs, field = "hrd_results", id.field = "pair", mc.cores = 1) {
    paths = pairs[[field]]
    paths = unique(paths[file.exists(paths)])
    goodpairs = pairs[pairs[[field]] %in% paths]
    allpr = unique(goodpairs[[id.field]])
    lst = mclapply(paths, grab.hrdetect.features, mc.cores = mc.cores, goodpairs = goodpairs, field = field, id.field = id.field)
    out = rbindlist(lst)
    out$fpair = factor(out[[id.field]], allpr)
    return(out)
}


grab.ot.features <- function(ent, id.field = "pair") {
    path = ent[["fpaths"]]
    if (file.exists(path)) {
        ot = readRDS(path)$expl_variables
        ot[[id.field]] = ent[["ids"]]
        return(ot)
    }
}


#' @name pairs.grab.ot.features
#' @title grab oneness twoness features from pairs table
#' 
#' @description
#' wrapper around grab.ot.features
#' 
#' @export pairs.grab.ot.features
pairs.grab.ot.features <- function(pairs, field = "oneness_twoness", id.field = "pair", mc.cores = 1) {
    fpaths = pairs[[field]]
    dt = data.table(
        fpaths,
        ids = pairs[[id.field]],
        fe = file.exists(fpaths)
    )
    dt = unique(dt[dt$fe == TRUE,])
    lst = dt %>% split_by("ids")
    lst.res = mclapply(lst, grab.ot.features, id.field = id.field, mc.cores = mc.cores)
    out = rbindlist(lst.res, fill = T)
    out$`NA` = NULL
    out$`"sample"` = NULL
    out$`.` = NULL
    return(out)
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
  thisenv = environment()
  iter.fun = function(pr, tbl) {
    try2({
      ent = tbl[get(id.field) == thisenv$pr]
      return(.filter_sv(ent))
    })
  }
  out = mclapply(mc.cores = mc.cores,
    tbl[[id.field]], iter.fun, tbl = tbl)
  out = tryCatch(rbindlist(out), error = function(e) {
    message("error at rbindlist, returning list"); out
  })
  return(out)
}


#' @export plot.jabba
plot.jabba = function(pairs, win, filename, use.jab.cov = TRUE, field.name = "jabba_rds", cov.field.name = "cbs_cov_rds", cov.y.field = "ratio", title = "", doplot = TRUE, gt, plotfun = "ppng", h = 10, w = 10, rebin = FALSE, binwidth = 1e3, lwd.border = 0.0001, ...) {
    if (is.character(plotfun)) {
        plotfun = get(plotfun)
    } else if (!is.function(plotfun)) {
        stop("plotfun needs to be a function")
    }
    if (missing(gt)) {
        gg = gG(jabba = pairs[[field.name]])
        if (isTRUE(use.jab.cov))
            cov = readRDS(diginjob(pairs[[field.name]])$CovFile)
            ## cov = readRDS(inputs(readRDS(pairs[[field.name]] %>% dig_dir("Job.rds$")))$CovFile)
        else
            cov = readRDS(pairs[[cov.field.name]])
        if (rebin)
            cov = rebin(cov, binwidth = binwidth)
        gcov = gTrack(cov, cov.y.field, circles = TRUE, lwd.border = lwd.border, y0 = 0)
        gt = within(c(gcov, gg$gtrack()), {y0 = 0})
    }
    if (missing(win))
        win = si2gr(hg_seqlengths()) %>% keepStandardChromosomes(pruning.mode = "coarse") %>% gr.sort
    if (isTRUE(doplot)) {
        if (missing(filename))
            plotfun(plot(gt, win = win, ...), res = 200, title = title, h = h, w = w)
        else
            plotfun(plot(gt, win = win, ...), filename = filename, res = 200, title = title, h = h, w = w)
    }
    return(gt)
}


#' @export pairs.plot.jabba
pairs.plot.jabba = function(pairs, dirpath = "~/public_html/jabba_output", jabba.field = "jabba_rds", cov.y.field = "foreground", id.field = "pair", mc.cores = 1) {
    paths = subset2(pairs[[jabba.field]], file.exists(x))
    iter.fun = function(x, tbl) {
        ent = tbl[get(jabba.field) == x]
        ttl = ent[[id.field]]
        plot.jabba(ent, use.jab.cov = TRUE, field.name = jabba.field, filename = paste0(dirpath, "/", ent[[id.field]], ".png"), cov.y.field = cov.y.field, y.quantile = 0.01, title = ttl)
    }
    mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores)
    NULL
}


#' @export pairs.process.events
pairs.process.events = function(pairs, events.field = "complex", id.field = "pair", mc.cores = 1) {
    paths = unique(subset2(pairs[[events.field]], file.exists(x)))
    iter.fun = function(x, tbl) {
        tryCatch({
            ent = pairs[get(events.field) == x]
            gg = readRDS(unique(ent[[events.field]]))
            out = copy(gg$meta$events)
            set(out, j = id.field, value = unique(ent[[id.field]]))
            out
        }, error = function(e) printerr(x))
    }
    lst = mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores)
    evs = rbindlist(lst, fill = TRUE)
    fid = evs[, factor(get(id.field), levels = unique(pairs[get(events.field) %in% paths][[id.field]]))]
    set(evs, j = paste0("f", id.field), value = fid)
}

#' @export pairs.process.rbp
pairs.process.rbp = function(pairs, rbp.field = "complex", id.field = "pair", mc.cores = 1) {
    tryCatch({
        path = pairs[[rbp.field]]
        if (!file.exists(path)) return(NULL)
        gg = readRDS(path)
        out = gg$meta$recip_bp
        dt.tic = gg$meta$tic
        if (!NROW(out)) out = data.table()
        if (NROW(dt.tic)) {
            dt.tic = dt.tic %>% rename_at(vars(-1), ~paste0("tic", "_", .))
            out = merge.repl(out,
                       dt.tic, by = "tic", all = T)
        }
        out$pair = pairs$pair
        return(out)
    }, error = function(e) printerr(pairs$pair))
}

#' @export process_for_rbp
process_for_rbp <- function(ent, field = "complex", id.field = "pair") {
    tryCatch({
        gg = readRDS(ent[[field]])
        out = gg$meta$recip_bp
        dt.tic = gg$meta$tic
        if (!NROW(out)) out = data.table()
        if (NROW(dt.tic)) {
            dt.tic = dt.tic %>% rename_at(vars(-1), ~paste0("tic", "_", .))
            out = merge.repl(out,
                       dt.tic, by = "tic", all = T)
        }
        out[[id.field]] = rep_len2(ent[[id.field]], out)
        return(out)
    }, error = function(e) printerr(ent[[id.field]]))
}

#' @export pairs.process.rbp
pairs.process.rbp <- function(pairs, field = "complex",
                              id.field = "pair", mc.cores = 1) {
  envr = environment()
  lg = which(file.exists(pairs[[field]]))
  ents = pairs[envr$lg,]
  lg.d = which(!duplicated(ents[[id.field]]))
  ents = ents[envr$lg.d,]
  lst = split(ents, ents[[id.field]])
  res = mclapply(lst, process_for_rbp,
           mc.cores = mc.cores,
           field = field, id.field = id.field)
  rbp = rbindlist(res, fill = T)
  fid.field = paste0("f", id.field)
  rbp[[fid.field]] = factor(rbp[[id.field]], levels = ents[[id.field]])
  return(rbp)
}


#' @export pairs.process.homeology
pairs.process.homeology = function(pairs, id.field = "pair", mc.cores = 1) {
    coolp = pairs[file.exists2(homeology) & file.exists2(homeology_stats)]$pair
    subp = pairs[coolp]
    ifun =  function(x, debug = FALSE) {
        try2({
            if (debug) browser()
            addon = c("iw", "jw", "r", "minpx")
            homout = fread(x$homeology)
            homstat = fread(x$homeology_stats)
            if (len(homstat) == 0) return(NULL)
            if (!all(colexists(addon, homstat))) {
                homstat = cbind(homstat, lapply_dt(addon, homstat))
            }
            out = merge.repl(homstat, homout[, .(seq, edge.id)], by = "seq")[, pair := x$pair]
            gri = parse.gr2(with(out, ifelse(nzchar(iw) & !is.na(iw), iw, "0:1-1")))
            grj = parse.gr2(with(out, ifelse(nzchar(jw) & !is.na(jw), jw, "0:1-1")))
            out[, atbp := gri %^% "Left:0-0" & grj %^% "Right:0-0"]
            ## thresh 8 levenshtein dist... 32 / 40 bases must match per window
            ## 80% similarity seems to work?
            ## what does it mean if you have 5 pixels of match?
            ## 5 + 40 bases of at least 80% sequence similarity
            ## using a pad of 20 means that we cannot look for stretches smaller than 40 bp...
            ## which is fine... it seems like that is a fine threshold to start at
            ## since we stopped looking at 40 bp...
            ## let's see what this distribution looks like
            out = out[, .(
                hlen = max(ifelse(na2false(r > 0.9), minpx, 0L)),
                hlen_be = max(ifelse(na2false(r > 0.9) & atbp, minpx, 0L))),
                by = .(pair, seq, edge.id)]
            return(out)
        })
    }
    out = rbindlist(mclapply(split(subp, subp$pair), ifun, mc.cores = mc.cores), fill = T)
    et(sprintf("out[, f%s := %s]", id.field, id.field))
    return(out)
}

#' @name pairs.collect.junctions
#' @title collect junctions from pairs as breakpoints
#'
#' @description
#' 
#' @export pairs.collect.junctions
pairs.collect.junctions <- function(pairs, jn.field = "complex", id.field = "pair", mc.cores = 1, mask = '/gpfs/commons/groups/imielinski_lab/DB/Broad/um75-hs37d5.bed.gz', ev.types = c("bfb", "chromoplexy", "chromothripsis", "del", "dm", "dup", "fbi", "pyrgo", "qrdup", "qrdel", "qrp", "rigma", "simple_inv", "simple_invdup", "simple_tra", "tic", "tyfonas", "cpxdm", "tib", "qrppos", "qrpmix", "qrpmin")) {
    paths = unique(subset2(pairs[[jn.field]], file.exists(x)))
    prs = unique(pairs[[id.field]][pairs[[jn.field]] %in% paths])
    mask = rtracklayer::import(mask)
    iter.fun <- function(x, tbl) {
        ent = tbl[get(jn.field) == x]
        ent = ent[!duplicated(ent[[id.field]]),,drop=F]
        .fun <- function(gg) {
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
        pr = unique(ent[[id.field]])
        ## pth = unique(ent[[jn.field]])
        message("processing ", pr)
        cx = readRDS(x)
        if (!length(cx)) return(NULL)
        cx = .fun(cx)
        cx$edges$mark(jspan = cx$edges$span)
        cx$edges$mark(shadow = grl.string(cx$edges$shadow %>% split(.$id)))
        cx$edges$mark(sv.in.mask = grl.in(cx$edges$grl, mask, logical = FALSE) > 0)
        ## cx$edges[edge.id %in% these_id]$mark(within_node_cluster = TRUE)
        ## these_id = cx$nodes[!is.na(cluster)]$edges$dt$edge.id
        out = copy(gr2df(grl.unlist(cx$edges[type == "ALT"]$grl)))
        if (!is.null(dim(out)) && !dim(out)[1] == 0) {
            set(out, j = "pair", value = pr)
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
            out = gr2df(gr.val(df2gr(out),
                               plyranges::select(cx$nodes$gr, bp_scn = cn), "bp_scn"))
            out = df2gr(out) %>% mutate(bp.in.mask = (.) %^% mask) %>% gr2df
        }
        badcols = which(sapply(out, function(x) inherits(x, c("AsIs", "List", "list"))))
        if (NROW(badcols)) out = out[, -badcols,with=F]
        return(out)
    }
    lst = mclapply(paths, iter.fun, tbl = pairs, mc.cores = mc.cores)
    cx.edt = rbindlist(lst, fill = TRUE)
    if (NROW(cx.edt) == 0) return(cx.edt)
    set(cx.edt, j = "fpair", value = factor(cx.edt[[id.field]], levels = prs))
    cx.edt = cx.edt[, !colnames(cx.edt) == "rowname", with = FALSE]
    cx.edt = merge.repl(cx.edt, unique(cx.edt[, .(pair, edge.id, simple_type = gsub("([A-Z]+)([0-9]+)", "\\1", simple), simple_num = gsub("([A-Z]+)([0-9]+)", "\\2", simple))]), by = c("pair", "edge.id"))
    cx.edt$simple_type = factor(cx.edt$simple_type, levels = c("INV", "INVDUP", "TRA"))
    cx.edt[, simple_type := fct_explicit_na(simple_type, "NA")]
    mod.dt = mltools::one_hot(cx.edt[, .(simple_type)])
    cx.edt = cbind(dplyr::select(cx.edt, -dplyr::matches("^simple_.*$")),
                   dplyr::rename_all(mod.dt, ~paste0("simple", gsub("simple_type", "", tolower(.)))))
    ## cx.mat = as.matrix(mutate_all(replace_na(cx.edt[, ev.types,with = FALSE], 0), as.numeric))
    cx.mat = as.matrix(dplyr::mutate_all(replace_na(dplyr::select(cx.edt, dplyr::one_of(ev.types)), 0), as.numeric))
    cx.mat = cx.mat > 0
    mode(cx.mat) = "integer"
    cx.edt[, unclassified := rowSums(cx.mat) == 0]
    return(withAutoprint(cx.edt, echo = F)$value)
}
## overwritefun("pairs.collect.junctions", "pairs.collect.junctions", "khtools")


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


#' @name forceload
#' @title force functions to load from all libraries
#'
#' @description
#' A function to evaluate all functions in all loaded
#' and attached packages to prevent errors upon reinstallation of
#' a package
#'
#' @param envir environment to evaluate in (probably doesn't matter)
#' @export
forceload = function(envir = globalenv()) {
    force = function(x) x
    pkgs = gsub("package:", "", grep('package:', search(), value = TRUE))
    pkgs = c(pkgs, names(sessionInfo()$loadedOnly))
    for (pkg in pkgs) {
        tryCatch( {
            message("force loading ", pkg)
            invisible(eval(as.list((asNamespace(pkg))), envir = envir))
            invisible(eval(eapply(asNamespace(pkg), force, all.names = TRUE), envir = envir))
        }, error = function(e) message("could not force load ", pkg))
    }
}


## forceload = function(envir = globalenv()) {
##     pkgs = gsub("package:", "", grep('package:', search(), value = TRUE))
##     pkgs = c(pkgs, names(sessionInfo()$loadedOnly))
##     for (pkg in pkgs) {
##         tryCatch( {
##             message("force loading ", pkg)
##             invisible(eval(as.list((asNamespace(pkg))), envir = envir))
##             invisible(eval(eapply(asNamespace(pkg), base::force, all.names = TRUE), envir = envir))
##         }, error = function(e) message("could not force load ", pkg))
##     }
## }

#' @name force2
#' @title force with a tryCatch
#'
#' @description
#' evaluate with tryCatch
#'
#' @param x an object
#' @export
force2 = function(x)
    tryCatch(x, error = function(e) NULL)

#' @name forcefun
#' @title force functions to load
#'
#' @description
#' A function to evaluate all functions in a single environment
#'
#' @param envir environment to grab all functions from
#' @param evalenvir environment to evaluate in (probably doesn't matter)
#' @export
forcefun = function(envir = globalenv(), evalenvir = globalenv()) {
    funnames = as.character(lsf.str(envir = envir))
    for (fun in funnames) {
        tryCatch( {
            message("force loading ", fun)
            eval(force(get(fun, envir = envir)), envir = evalenvir)
        }, error = function(e) message("could not force load ", fun))
    }
}

#' @name forceall
#' @title force objects (including functions) to evaluate from environment
#'
#' @description
#' A function to evaluate all objects in an environment
#' to be used within a function or some other environment
#'
#' @param invisible logical whether to print the objects in the environmnet or not
#' @param envir environment with objects to evaluate
#' @param evalenvir environment to evaluate in (probably doesn't matter)
#'
#' @export
forceall = function(invisible = TRUE, envir = parent.frame(), evalenvir = parent.frame()) {
    if (invisible)  {
        invisible(eval(as.list(envir), envir = evalenvir))
        invisible(eval(eapply(envir, force2, all.names = TRUE), envir = evalenvir))
    } else {
        print(eval(as.list(envir), envir = evalenvir))
        print(eval(eapply(envir, force2, all.names = TRUE), envir = evalenvir))
    }
}

#' @name asn2
#' @title version of utils::assignInNamespace
#'
#' @description
#' can be used to reassign function into a namespace
#' USE WITH CAUTION
#'
#' @export
asn2 = function (x, value, ns, pos = -1, envir = as.environment(pos)) {
    nf <- sys.nframe()
    if (missing(ns)) {
        nm <- attr(envir, "name", exact = TRUE)
        if (is.null(nm) || substr(nm, 1L, 8L) != "package:")
            stop("environment specified is not a package")
        ns <- asNamespace(substring(nm, 9L))
    }
    else ns <- asNamespace(ns)
    ns_name <- getNamespaceName(ns)
    ## if (nf > 1L) {
    ##     if (ns_name %in% tools:::.get_standard_package_names()$base)
    ##         stop("locked binding of ", sQuote(x), " cannot be changed",
    ##             domain = NA)
    ## }
    if (bindingIsLocked(x, ns)) {
        in_load <- Sys.getenv("_R_NS_LOAD_")
        if (nzchar(in_load)) {
            if (in_load != ns_name) {
                msg <- gettextf("changing locked binding for %s in %s whilst loading %s",
                  sQuote(x), sQuote(ns_name), sQuote(in_load))
                if (!in_load %in% c("Matrix", "SparseM"))
                  warning(msg, call. = FALSE, domain = NA, immediate. = TRUE)
            }
        }
        else if (nzchar(Sys.getenv("_R_WARN_ON_LOCKED_BINDINGS_"))) {
            warning(gettextf("changing locked binding for %s in %s",
                sQuote(x), sQuote(ns_name)), call. = FALSE, domain = NA,
                immediate. = TRUE)
        }
        unlockBinding(x, ns)
        assign(x, value, envir = ns, inherits = FALSE)
        w <- options("warn")
        on.exit(options(w))
        options(warn = -1)
        lockBinding(x, ns)
    }
    else {
        assign(x, value, envir = ns, inherits = FALSE)
    }
    if (!isBaseNamespace(ns)) {
        S3 <- .getNamespaceInfo(ns, "S3methods")
        if (!length(S3))
            return(invisible(NULL))
        S3names <- S3[, 3L]
        if (x %in% S3names) {
            i <- match(x, S3names)
            genfun <- get(S3[i, 1L], mode = "function", envir = parent.frame())
            if (.isMethodsDispatchOn() && methods::is(genfun,
                "genericFunction"))
                genfun <- methods::slot(genfun, "default")@methods$ANY
            defenv <- if (typeof(genfun) == "closure")
                environment(genfun)
            else .BaseNamespaceEnv
            S3Table <- get(".__S3MethodsTable__.", envir = defenv)
            remappedName <- paste(S3[i, 1L], S3[i, 2L], sep = ".")
            if (exists(remappedName, envir = S3Table, inherits = FALSE))
                assign(remappedName, value, S3Table)
        }
    }
    invisible(NULL)
}

#' @name ww
#' @export
ww = with

#' @name wn
#' @export
wn = within


## .onLoad = function(libname, pkgname) {
##     message("khtools forcing functions to evaluate on load...")
##     forceall(T, envir = asNamespace("khtools"), evalenvir = globalenv())
## }

## .onAttach = function(libname, pkgname) {
##     message("khtools forcing functions to evaluate on attach...")
##     forceall(T, envir = asNamespace("khtools"), evalenvir = globalenv())
## }
