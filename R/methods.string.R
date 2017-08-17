#' \code{methods.string} package
#'
#' methods.string
#'
#' See the README on
#'
#' @docType package
#' @name methods.string
#' @importFrom dplyr %>%
#' @importFrom data.table :=
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))

#' @title clean.city
#'
#' @description Extracts and cleans city from cityStateZip based on table lookup and matching.
#' @param cityStateZip character vector
#' @param study.cities character vector
#' @keywords city, clean
#' @export
#' @import stringr
#'     data.table
#'     stringdist
clean.city <- function(cityStateZip, study.cities=NULL){
    if(is.null(study.cities)){
        study.cities <- methods.string::study.cities
    }
    city.replace <- NULL
    city <- NULL
    # city state zip
    cityStateZip <- str_trim(cityStateZip)
    cityStateZip <- str_replace_all(cityStateZip, ',|-|\\(|\\)', ' ')
    cityStateZip <- str_replace_all(cityStateZip, regex('(?i)(?<=^)ENVER', perl=TRUE), 'Denver')
    x <- data.table(cityStateZip)
    regex.remove <- '(?i)( [A-Z]{2,2}( |$|,|, )|[0-9]{1,5})(.*.|$)'
    x <- x[, city:=str_replace_all(cityStateZip, regex(regex.remove, perl=TRUE), '')]
    x <- x[, city:=str_trim(str_to_title(city))]
    # Do some string matching
    city.search <- unique(x$city)
    match.ids<-amatch(city.search, study.cities, method='soundex')
    cities.soundex <- study.cities[match.ids]
    cities.jw <- study.cities[amatch(city.search, study.cities, method='jw')]
    city.table <- data.table(city.search, cities.soundex, cities.jw)
    city.table <- city.table[, city.replace:=cities.jw]
    city.table <- city.table[is.na(cities.jw), city.replace:=cities.soundex]
    city.table <- unique(city.table[, .(city.search, city.replace)][city.search!='' & city.search != 'Na'])
    city.table <- city.table[order(city.search)]
    missing.cities <- city.table[is.na(city.replace), city.search]
    city.table <- city.table[!is.na(city.replace)]
    matches.pattern <- paste0('(?i)(?<=^)',city.table$city.search)
    matches.replacement <- city.table$city.replace
    if(length(matches.replacement>0)){
        names(matches.replacement) <- matches.pattern
        cityStateZip <- str_replace_all(cityStateZip, matches.replacement)
    }
    cityStateZip <- str_replace_all(cityStateZip, regex('[ ]{1,}', perl=TRUE), ' ')
    return(cityStateZip)
}
#' @title clean.lic.type
#'
#' @description cleans marijuana license type for mmed locations
#' @param license.type character vector
#' @keywords city, license, lic, type
#' @export
#' @import stringr
#'     data.table
clean.lic.type <- function(license.type){
    license.type <- str_trim(str_to_title(license.type))
    license.type <- gsub('Mmj Center License ', 'Center - ', license.type)
    return(license.type)
}
#' @title clean.lic.num
#'
#' @description cleans marijuana license number
#' @param license.num character vector
#' @keywords city, license, lic, num, number
#' @export
#' @import stringr
#'     data.table
clean.lic.num <- function(license.num){
    license.num <- str_trim(str_to_title(license.num))
    return(license.num)
}
#' @title clean.name
#'
#' @description Clean an d sanitize business (location) names
#' @param name character vector with names
#' @keywords clean anems
#' @export
#' @import stringr
clean.name <- function(name){
    name <- gsub('THE PLEASURE CAF\x83 LLC', 'THE PLEASURE CAFE LLC', name)
    name <- str_trim(gsub('\\&.*.\\;', "'", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub('\\&.*.\\;', "'", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub(',|\\.', "", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub('Inc\\_', "Inc", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub("'", "", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub("\\-", "", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub('[^[:alnum:] ]', "", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub('[\\_]{2,}', '', name, perl=TRUE))
    name <- str_trim(gsub('( llc| inc)(?= |$)', " ", name, ignore.case = TRUE, perl=TRUE))
    name <- gsub('\\s{2,}',' ', name, perl=TRUE)
    name <- gsub('\\.','', name, perl=TRUE)
    name <- str_to_title(name)
    return(name)
}
#' @title clean.name.key
#'
#' @description create a unique name key
#' @param name character vector with names
#' @keywords clean anems
#' @export
#' @import stringr
clean.name.key <- function(name){
    name <- str_trim(gsub('\\&.*.\\;', "'", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub(',|\\.', "", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub('Inc\\_', "Inc", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub("'", "", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub("\\-", " ", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub('[^[:alnum:] ]', "", name, ignore.case = TRUE, perl=TRUE))
    name <- str_trim(gsub('[\\_]{2,}', '', name, perl=TRUE))
    name <- str_trim(gsub('( llc| inc)(?= |$)', " ", name, ignore.case = TRUE, perl=TRUE))
    name <- gsub("'", '', name)
    name <- gsub("\\.", '', name)
    name <- gsub("\\.", '', name)
    name <- tolower(gsub('[^[:alnum:]]', '', name))
    name <- gsub("llc|group|cooperative", '', name)
    name <- gsub("coop|center|industry|industries|inc|amp", '', name)
    name <- gsub("the ", '', name)
    name <- str_to_lower(name)
    return(name)
}

#' @title clean.special
#'
#' @description Cleans up some custom strings in the c*star rent data.
#' @param DT datatable with address info, city, street.type, street.direction.prefix, street.body
#' @param DT.name string flag for c*rent
#' @keywords special clean for some weird exceptions (Denver does not have any north)
#' @export
#' @import data.table
clean.special <- function(DT, DT.name=''){
    data.id <- NULL
    city <- NULL
    street.type <- NULL
    street.direction.prefix <- NULL
    street.body <- NULL
    city <- NULL
    street <- NULL
    if (DT.name=='costar.rent'){
        DT <- DT[data.id == '1657', city := 'Denver']
        glendale.building.id <- c('105','148', '152', '153', '159')
        DT <- DT[data.id %in% glendale.building.id, city:='Denver']
        DT <- DT[data.id=='368', street.type:='Ave']
        DT <- DT[data.id=='502', street.type:='Pl']
        DT <- unique(DT)
    }
    DT <- DT[city=='Denver' & street.direction.prefix =='N', street.direction.prefix:='']
    DT <- DT[city=='Denver' & street.body == 'Broadway', street.type:='']
    DT <- DT[street=='6859 N Hills Hwy Building C', street.body:= 'Foothills Hwy']
    DT <- DT[street=='4699 Nautilus Ct South', city:='Boulder']
    DT <- DT[street=='4301 Jason Street Suite 101', street.type:='Ct']
    return(DT)
}
#' @title clean.state
#'
#' @description Just returns CO if state is missing.
#' @param cityStateZip character vector
#' @keywords silly, colorodo
#' @export
#' @import stringr
#'     data.table
#'     stringdist
clean.state <- function(cityStateZip){
    state.name <- NULL
    name <- NULL
    state.replace <- NULL
    x <- cityStateZip
    x <- str_extract(x, regex('(?<= ).+', perl=TRUE))
    states <- unlist(str_split(x, regex(', ', perl=TRUE)))
    # Those without abbreviations
    state.search <- unique(states)
    state.search <- state.search[str_length(state.search)>2 & !grepl('[0-9]{1,}', state.search)]
    states.soundex <- methods.string::lookup.state[amatch(state.search, name, method='soundex')]$abb
    states.jw <- methods.string::lookup.state[amatch(state.search, name, method='jw')]$abb
    state.table <- data.table(state.search, states.soundex, states.jw)
    state.table <- state.table[, state.replace:=states.jw]
    state.table <- state.table[is.na(states.jw), state.replace:=states.soundex]
    state.table <- unique(state.table[, .(state.search, state.replace)][state.search!='' & state.search != 'Na'])
    state.table <- state.table[order(state.search)]
    missing.states <- state.table[is.na(state.replace), state.search]
    state.table <- state.table[!is.na(state.replace)]
    matches.pattern <- paste0('(?<= )',state.table$state.search, '(?=( |,|$))')
    matches.replacement <- state.table$state.replace
    if(length(matches.replacement>0)){
        names(matches.replacement) <- matches.pattern
        cityStateZip <- str_replace_all(cityStateZip, matches.replacement)
    }
    cityStateZip <- str_replace_all(cityStateZip, regex('[ ]{1,}', perl=TRUE), ' ')
    return(cityStateZip)
}
#' @title clean.street
#'
#' @description Cleans up and standardizes street string. Needed for the street.explode function.
#' @param street character vector of streets
#' @param l.regex helper function since data.table not parallelizable
#' @keywords street.explode
#' @export
#' @import stringr
#'     data.table
#'     stats
clean.street <- function(street, l.regex){
   street.base <-''
    iter <- 0
    while(identical(street, street.base)==FALSE & iter < 6){
        #changes <- cbind(street.base[which(street !=street.base)],street[which(street !=street.base)])
        street.base <- street
        iter <- iter + 1
        street <- str_trim(str_replace_all(street, regex('[\\s]{2,}', perl=TRUE), ' '))
        street <- str_replace(street, '\\\\', '')
        street <- str_replace(street, ' Units ', ' Unit ')
        street <- str_replace(street, ' Suites ', ' Suites ')
        street <- str_replace(street, 'Unitis', 'Unit')
        street <- str_replace(street, '1529 York St 2Nd Fl', '1529 York St')
        street <- str_replace(street, 'South Boulder', 'SouthBoulder')

        #street <- '28 StandBrighton Blvd'
        street <- str_replace(street,regex(l.regex$intersection, perl=TRUE), ' and ')

        #E 17Th Ave @ Park Avenue West'
        street <- str_replace(street, '@', 'and')

        #Ste 300 425 S Wilcox St', 'Suite 2400 1600 Broadway'
        street <- str_replace(street, regex(l.regex$unit.start, perl=TRUE), '')

        #One Flatiron Circle Ste Ff232'
        str.nums <- as.character(1:10)
        names(str.nums) <-  paste0('(?i)(?<=^)',c('one', 'two', 'three', 'four', 'five', 'six', 'seven', 'eight', 'nine', 'ten'))
        street <- str_replace_all(street, str.nums)

        # MMED fixed
        #  1044 & 1046 W 5TH AVE
        street <- str_replace(street, regex(paste0('(?<=[0-9]{1,1})', '( |)&( |)', '(?=[0-9]{1,})'), perl=TRUE), '-')
        street <- str_replace(street, '1545 W BAYAUD STREET #3', '1545 W BAYAUD Ave #3')
        street <- str_replace(street, 'FILMORE', 'Fillmore')
        street <- str_replace(street, '1136 N YUMA CT', '1136 Yuma CT')
        street <- str_replace(street, '150 RIO GRAND BLVD', '150 RIO GRANDE BLVD')
        street <- str_replace(street, '7739 E -7741 COLFAX AVE','7739-7741 E COLFAX AVE')

        #9800 Mt Pyramid Ct Ste 4042Aand4042b # Be careful to not break up "3415 S Oleander"
        unit.parts <-  str_split(street, '\\s')
        unit.parts.and <- lapply(unit.parts, str_match, pattern=regex('.*and.*', perl=TRUE))
        unit.parts.and <- sapply(sapply(unit.parts.and, str_match, pattern=regex('.*\\d{1,}.*', perl=TRUE), simplify=TRUE), na.omit, simplify=TRUE)
        unit.parts.and <- sapply(unit.parts.and, unlist, simplify=TRUE)
        unit.street.edit <- which(sapply(unit.parts.and, length)>0)
        street[unit.street.edit] <- lapply(street[unit.street.edit], str_replace, regex('(?<=[^\\s])and(?=[^\\s])', perl=TRUE), ' and ')

        # Remove special characters and periods (keep extra space until end)
        #2517 1/2 BROADWAY'
        street <- str_replace(street,regex('(?<=[0-9]{1,1}) [0-9]{1,}\\/[0-9]{1,} (?=[A-z]{1,1})', perl=TRUE), ' ')
        street <- str_to_title(gsub('\\,|\\#|;', ' ', street))
        street <- str_replace_all(street, '((\\s){0,}\\&(\\s{0,}))',' and ')
        street <- str_replace_all(street, '\\.|\\(|\\)','')
        street <- str_replace_all(street, '\\*', '')
        # Clean up
        #"3740B E 40Th Ave"
        street <- str_replace_all(street,'\\^', '')
        street <- str_replace(street,'SuiteC & D', 'Suite C&D')
        street <- str_replace(street,'StreetSuite', 'Street Suite')
        street <- str_replace(street,'(?i)Unit(-|6)', 'Unit')
        street <- str_replace(street,'UNITA&B', 'UNIT A&B')
        street <- str_replace(street,'SUITE100', 'Suite 100')
        street <- str_replace(street,'1910 - 16th', '1910 16th')
        street <- str_replace(street,'St Paul', 'Saint Paul')
        street <- str_replace(street,'StreetPenrose', 'Street Penrose')
        street <- str_replace(street,'A150', 'Unit A150')
        street <- str_replace(street,' Cr ', ' County Road ')
        street <- str_replace(street,' U S ', ' US ')
        street <- str_replace(street,' Us ', ' US ')
        street <- str_replace(street,'Btm \\& Top', '')
        street <- str_replace(street,'First Floor', 'Floor 1')
        street <- str_replace(street, regex('[\\s]{0,}(\\+|\\-|\\/|\\&)[\\s]{0,}', perl=TRUE), '-')
        street <- str_replace(street, regex('(?<=[0-9]{1,1})\\-(?=[0-9]{1,}[A-z]{1,}\\s)', perl=TRUE), ' ')
        street <- str_replace(street, regex('[A-z]{1,}(\\-)(?=[0-9]{0,}$)', perl=TRUE), ' ')
        street <- str_replace(street, 'Jewll', 'Jewell')
        street <- str_replace(street,'Bottom-Top', '')
        street <- str_replace(street,'A-Ste F1', 'A')
        street <- str_replace(street,' Frnt St ', ' Front St ')
        street <- str_trim(str_replace_all(street, regex('[\\s]{2,}', perl=TRUE), ' '))
        #"1997 And 2000 S Acoma St"'
        street <- str_replace(street,regex('(?<=[0-9]{1,1}) And (?=[0-9]{1,1})', perl=TRUE), '-')
        #990W 6Th Ave Unit 5'
        str.extract <- str_extract(street,regex(paste0('(?<=^)[0-9]{1,}(',l.regex$abbrev.compass.search,' )'), perl=TRUE))
        str.direction <- str_extract(str.extract,regex(paste0('(',l.regex$abbrev.compass.search,')'), perl=TRUE))
        street <- str_replace(street, regex(paste0(str.direction),perl=TRUE), paste0(' ', str.direction))
        #"3740B E 40Th Ave"
        # bad.prefix <- str_detect(street, regex('(?<=^)([0-9]{1,}[A-z]{1,})(?=\\s)', perl=TRUE))
        street.num.prefix <- str_extract(street, regex('(?<=^)([0-9|-]{1,}[A-z]{0,})(?=\\s)', perl=TRUE))
        street.num.prefix[is.na(street.num.prefix)] <- ''
        street.num.extract <- str_extract(street, regex('(?<=^)([0-9|-]{1,})(?=(([A-z]{1,1})|\\s|$))', perl=TRUE))
        street.num.extract[is.na(street.num.extract)] <- ''
        street.body <- str_replace(street, paste0('(?<=^)',street.num.prefix, ' '), '')
        street <- paste(street.num.extract, street.body)
        # street.num.prefix[is.na(street.num.prefix)] <- street.num.extract
        # street.unit.suffix <- str_replace(street.num.extract, street.num.prefix, '')
        # street <- paste(street.num.prefix, street.body, street.unit.suffix)
        #9900 Stanford Ave St 107'
        street <- str_replace(street, ' Ave St ', ' Ave Ste ')
        street <- str_replace(street, 'Royal M H', 'Royal MH')
        street <- str_replace(street, '6859 N Hills Hwy', '6859 N Foothills Hwy')
        #5401 S Park Terrace Ave Ste 209 A'
        street <- str_replace(street, regex('(?<=[0-9])\\s(?=\\w$)', perl=TRUE), '')
        #1035 Pearl St 3rd Floor'
        street <- str_replace(street,  '1035 Pearl St 3rd Floor', '1035 Pearl St Floor 3')
        street <- str_replace(street, '2625 East Street Vrain A',  '2625 East St Vrain Street A')
        # abbrev.suffix <- methods.string::abbrev[class=='suffix', search]
        # regex.abbrev.suffix <- paste0('(?<= )(', paste0(abbrev.suffix, collapse='|'), ')(?= )')
        # # Speed up
        # abbrev.suffix <- unique(unlist(str_extract_all(street, regex(regex.abbrev.suffix, perl=TRUE))))
        # if (length(abbrev.suffix>0)){
        #     regex.suffix <- paste0('(', paste0(abbrev.suffix, collapse='|'), ')')
        #     regex.suffix.pair <- paste0(regex.suffix, '\\s', regex.suffix)
        #     str.search <- unique(str_extract(street,regex(regex.suffix.pair, perl=TRUE)))
        #     str.search <- str.search[!is.na(str.search)]
        #     str.replace <- str_extract(str.search,regex(paste0('(?<=^)', regex.suffix, '(?= )'), perl=TRUE))
        #     names(str.replace) <- str.search
        #     street <- str_replace_all(street, str.replace)
        # }
        # Step 3: Get rid of extra spaces and return

        # Remove two unit suffixes
        reg.unit <- paste0('(',paste0(paste0(' ',l.regex$x, ' '), collapse='|'),')')
        ids <- which(sapply(str_split(street, reg.unit), length)>2)
        pattern.neg.ahead <- paste0('(?!.* ',paste0(l.regex$s, collapse='|'),').*.')
        street[ids] <- str_trim(str_replace(street[ids], regex(paste0(str_extract(street[ids], regex(pattern.neg.ahead, perl=TRUE)),'(?=$)'), perl=TRUE),''))

        # street <- '4228 York Street Unit 104 106-107'
        pattern.neg.ahead <- paste0(l.regex$t,'(?!.*',l.regex$u,')(.*.|$)')
        street.tail <- str_extract(street, regex(pattern.neg.ahead, perl=TRUE))
        ids <- which(sapply(sapply(street.tail, str_split, ' |-| and '), length) > 2)

        if (length(ids) > 0){
            # print(paste('Cleaning', length(ids), 'bad street units'))
            for (i in 1:length(ids)){
                i.id <- ids[i]
                name.id <- names(i.id)
                search.sub <- paste0(name.id,'(?=$)')
                replace.sub <- clean.street.unit(name.id)
                names(replace.sub) <- search.sub
                # street.replace <- sapply(names(ids), clean.street.unit)
                street[i.id] <- str_replace_all(street[i.id], replace.sub)
            }
        }
        # street <- '2675 South Santa Fe Drive Building 6 Unit F Gandh' abc(?!.*abc)
        pattern.neg.ahead <- paste0(l.regex$s,'(?!.*',l.regex$s,')')
        pattern.detect <-'.*[\\w]{0,1}and[\\w]{0,1}.*'
        pattern <- paste0(pattern.neg.ahead, pattern.detect)
        step.1 <- str_extract(street, regex(pattern, perl=TRUE))
        ids <- which(str_detect(step.1, regex(paste0('(?<=',l.regex$s,') '), perl=TRUE)))
        if (length(ids)>0){
            street.sub <- street[ids]
            extract.sub <- str_extract(street.sub, regex(pattern, perl=TRUE))
            replace.sub <- str_extract(extract.sub, regex(paste0(l.regex$s, '( [:alnum:])'),perl=TRUE))
            search.sub <- paste0(extract.sub,'(?=$)')
            names(replace.sub) <- search.sub
            street[ids] <- str_replace_all(street[ids], replace.sub)
        }

        # Fix leading street num ranges
        # costar.rent.scrape: "521-& 541 14th St"
        nums.amp <- str_extract(street, regex('(?<=^)[0-9]{1,}- [0-9]{1,}'))
        ids <- which(str_detect(street, nums.amp))
        street[ids] <- str_replace_all(street[ids], '- (?=[0-9]{1,1})', '-')

        nums <- sapply(str_split(street,' '),'[',1)
        nums.split <- sapply(nums, str_split, pattern='-')
        ids <- which(sapply(nums.split, length)>1)
        ids <- ids[which(!str_detect(names(ids),'Highway|Hwy'))]
        if (length(ids)>0){
            # print(paste('Checking and fixing', length(ids), 'street number ranges'))
            replace.sub <- clean.street.num(names(ids))
            names(replace.sub) <- paste0('(?<=^)', names(ids))
            street[ids] <- str_replace_all(street[ids], replace.sub)
            street <- str_trim(str_replace_all(street, regex('[\\s]{2,}', perl=TRUE), ' '))
        }
    }
    if (iter==6) print(paste0('Error with clean.street',  cbind(street.base[which(street !=street.base)],street[which(street !=street.base)])))
    street <- str_trim(str_replace(street, regex('(?i)( LLC)', perl=TRUE), ' LLC'))
    return(street)
}
#' @title clean.street.regex
#'
#' @description Builds regex used in clean.street (depends on data.table which had mclapply bug in R 3.4.0)
#' @keywords clean street regex
#' @export
#' @import stringr
#'     data.table
#'     stats
clean.street.regex <- function(){
  class <- NULL; search <- NULL; collapse <- NULL
  l.regex <- list()
  l.regex$intersection <- paste0('(?<= ',paste0(methods.string::abbrev[class=='suffix' & search!='Wy', search], collapse='|'),')and')
  units<- paste0(methods.string::abbrev[class=='unit' & search != '#',search],collapse='|')
  l.regex$unit.start <- paste0('(?i)(?<=^)(',units,')\\s[0-9]{1,}\\s(?=[0-9]{1,1})')
  l.regex$abbrev.compass.search <- paste0(methods.string::abbrev[class=='compass',search], collapse='|')
  l.regex$s <- paste0('(',paste0(paste0(unlist(methods.string::abbrev[class!='compass'&search!='#', .(search, paste0(search,'s'))])),collapse='|'),')')
  l.regex$x <- sapply(t(methods.string::abbrev[class=='unit' & search!='#', .(search, paste0(search,'s'))]), cbind, simplify=TRUE)
  l.regex$t <- paste0('(',paste0(paste0(sort(c(unlist(methods.string::abbrev[class!='compass'&search!='#', .(search, paste0(search,'s'))]), 'Highway', 'Broadway'))),collapse=' |'),')')
  l.regex$u <- paste0('(',paste0(paste0(sort(c(unlist(methods.string::abbrev[class!='compass'&search!='#', .(search, paste0(search,'s'))]), 'Highway', 'Bay', 'Broadway'))),collapse='|'),')')
  return(l.regex)
}
#' @title clean.street.num
#'
#' @description Cleans up street.num, makes sure that hypens are set correctly.
#' @param street.num character vector of street.nums
#' @keywords street.num
#' @export
#' @import stringr
#'     data.table
clean.street.num <- function(street.num){
    high <- NULL
    low <- NULL
    diff.int <- NULL
    lead.1 <- NULL
    nchar.high <- NULL
    num.2.new <- NULL
    char.diff <- NULL
    num.split<-str_split(street.num, pattern='-')
    num.1 <- sapply(num.split, '[', 1)
    num.2 <- sapply(num.split, '[', 2)
    nums <- data.table(low=num.1, high=num.2)
    # 103-
    nums <- nums[high=='', high:=low]
    id.weird <- as.integer(nums$low)>as.integer(nums$high)
    # 10371-103
    nums$nchar.high <- nchar(nums$high)
    nums <- nums[id.weird & str_sub(low, 1, nchar.high) == high, high:=low]
    id.weird <- as.integer(nums$low)>as.integer(nums$high)
    # 3740-70
    nums <- nums[ , char.diff := nchar(low) - nchar(high)]
    nums <- nums[ , lead.1 := str_sub(low,1,char.diff)]
    nums <- nums[ ,  num.2.new := paste0(lead.1, high)]
    nums <- nums[,   diff.int := as.integer(num.2.new)-as.integer(low)]
    nums <- nums[diff.int>0 & diff.int < 200 & id.weird, high := num.2.new]
    id.weird <- as.integer(nums$low)>as.integer(nums$high)
    nums[id.weird]
    # 1565-1557
    nums <- nums[id.weird & diff.int < 0 & diff.int > -100, high:=low]
    nums <- nums[id.weird & diff.int < 0 & diff.int > -100, low:=num.2.new]
    street.num <- paste(nums$low,nums$high, sep='-')
    return(street.num)
}
#' @title clean.street.unit
#'
#' @description Cleans up street.unit number.
#' @param vec.chars character vector of street.units. This function is very confusing
#' @keywords street.num
#' @export
#' @import stringr
#'     data.table
#'     utils
clean.street.unit <- function(vec.chars){
    # print(vec.chars)
    if (grepl("Cir 7-O", vec.chars)) {
        vec.chars <- 'Cir 7-0'
        return(vec.chars)
    }
    l <- list(iterates='', leads='', repeats='')
    check <- list()
    #suffix.done <- FALSE
    vec.chars <- str_replace(vec.chars, ' and | \\+ ', ' ')
    x <- unlist(str_split(vec.chars,' |-| and '))
    l$prefix <- x[1]
    x <- x[2:length(x)]
    n.x <- length(x)
    x.nums <- table(unlist(strsplit(x, split='[A-z]{1,}')),exclude = '')
    #n.nums <-length(x.nums)
    x.chars <- table(unlist(strsplit(x, split='[0-9]{1,}')),exclude = '')
    #n.chars <- length(x.chars)
    l.mix <- list()
    if (length(x.nums)>0) {
        l.mix$x.nums <- x.nums
    }
    if (length(x.chars)>0){
        l.mix$x.chars <- x.chars
    }
    # vec.chars <- 'Unit F G H', 'Drive 6F 6G 6H', 'Drive A1 A2 A3' 'Suite 120 130 140'
    check$iterates <- which(sapply(l.mix, length)==n.x)
    if (length(check$iterates)>0){
        vals <- l.mix[[check$iterates]][l.mix[[check$iterates]]==1]
        if(length(vals)==n.x){
            chars <- sapply(names(l.mix[[check$iterates]]), nchar)
            combs <- t(combn(chars,2))
            diff <- combs[,2]-combs[,1]
            if(max(diff) == 0){
                l$iterates <- names(l.mix[[check$iterates]])
            } else {
                l$iterates <- NULL
                return(vec.chars)
            }
        }
    }
    v.mix <- unlist(l.mix)
    names(v.mix) <- sapply(str_split(names(v.mix),'\\.'),function(x) x[length(x)])
    leadstring <- names(l.mix$x.chars[1])
    greg.match <- sapply(names(v.mix), function(x) gregexpr(pattern=x, leadstring),simplify=TRUE)
    if (length(x.chars)==0){
        return(vec.chars)
    } else {
        check$leads <- which.max(sapply(greg.match[greg.match==1], function(x) attr(x,'match.length')))
        if (length(check$leads)>0){
            if(names(check$leads) %in% l$iterates){
                check$leads<-''
            }
            if(check$leads==1){
                if(names(check$leads)==leadstring){
                    l$leads <- names(check$leads)
                } else {
                    l$repeats <- names(check$leads)
                }
            }
        }
        rep.checks <- unlist(sapply(l.mix, function(x) x==n.x))
        names(rep.checks) <- sapply(str_split(names(rep.checks),'\\.'),function(x) x[length(x)])
        check$repeats <- which(rep.checks)
        if (length(check$repeats)>0){
            l$repeats <- names(check$repeats)
        }
        x.tail <- paste0(l$repeats, l$iterates[c(1,length(l$iterates))], collapse='-')
        if(nchar(x.tail)==1){
            x.tail<-''
        }
        x.out <- paste0(l$prefix, ' ',l$leads, ' ', x.tail)
        x.out <- str_trim(str_replace_all(x.out, '(\\s){1,}', ' '))
        return(x.out)
    }
}
#' @title explode.address
#'
#' @description Explodes a single string addresss using explode.street and explode.cityStateZip.
#' @param DT data.table needs to have fields street, cityStateZip
#' @param study.cities character vector
#' @param lookup.address optional table from main data usually parcels.address
#' @keywords street.num
#' @export
#' @import stringr
#'     data.table
#'     tidyr
#' @importFrom tidyr gather
explode.address <- function(DT, study.cities, lookup.address=NULL){
    street.num <- NULL
    street.num.low <- NULL
    street.num.hi <- NULL
    street.num.range <- NULL
    # Break out cityStateZip
    cityStateZip <- explode.cityStateZip(DT, study.cities)
    DT$cityStateZip <- NULL
    DT <- data.table(DT, cityStateZip)
     # Break out street
    street <- DT$street
    DT$street <- NULL
    street.explode <- explode.street(street)
    # Bring it back together (remove old street name from DT)
    DT <- data.table(DT, street.explode)
    DT <- DT[, street.num:=as.integer(street.num)]
    DT$street.num <- NULL
    DT <- tidyr::gather(DT, street.num.range, street.num, street.num.low:street.num.hi)
    DT <- as.data.table(DT)
    DT$street.num.range <- NULL
    DT$street <- NULL
    DT <- unique(DT)
    if(!is.null(lookup.address)){
        DT <- fill.missing.zip.city(DT, lookup.address) # from erikbjohn/location package
    }
    return(DT)
}
#' @title explode.cityStateZip
#'
#' @description Explodes cityStateZip into city state and zip
#' @param DT data.table with column name containg address or cityStateZip
#' @param study.cities character vector
#' @keywords street.num
#' @export
#' @import stringr
#'     data.table
#'     stats
explode.cityStateZip <- function(DT){
    regex.study.cities <- paste0(study.cities, collapse='|')
    x <- DT
    address.cols <- na.omit(str_extract(names(x), regex('(?i)(?=^)(address$|cityStateZip)', perl=TRUE)))
    address.col <- names(which.max(lapply(x[,(address.cols),with=FALSE], function(y) length(na.omit(y)))))[1]
    x <- x[, (address.col), with=FALSE]
    # Clean state (must do this first)
    x$cityStateZip <- clean.state(x$cityStateZip)
    # Clean city name
    x$cityStateZip <- clean.city(x$cityStateZip, study.cities)
    # First pass: Extract to the city
    # Find last 'city' location in string
    # abc(?!.*abc)
    pattern.neg.ahead <- paste0('(?i)(',regex.study.cities,')(?!.*(',regex.study.cities,'))(.*.|$)')
    cityStateZip <- as.data.table(sapply(x,function(y) str_extract(y,pattern=regex(pattern.neg.ahead, perl=TRUE))))
    pattern.city <- paste0('(?i)(',regex.study.cities,')(?!.*(',regex.study.cities,'))')
    city <- data.table(sapply(cityStateZip,function(y) str_extract(y,pattern=regex(pattern.city, perl=TRUE)),simplify=TRUE, USE.NAMES = FALSE ))
    setnames(city, names(city), 'city')
    zip <- sapply(cityStateZip, extract.zip, USE.NAMES = FALSE)
    state.regex <- paste0('(?= )', paste0(methods.string::lookup.state$abb, collapse='|'), '(?=( |,))')
    state <- str_extract(cityStateZip, regex(state.regex, perl=TRUE))
    x <- data.table(city, state, zip)
    setnames(x, names(x), c('city', 'state', 'zip'))
    return(x)
}
#' @title explode.street
#'
#' @description Explodes street, into component pieces for matching, return a data.table.
#' @param street charcater vector with address
#' @keywords explode, street
#' @export
#' @import stringr
#'     data.table
#'     parallel
explode.street <- function(street){
    parcels.street.last <- NULL
    street.body <- NULL
    street.cum <- NULL
    street.direction.prefix <- NULL
    street.direction.suffix <- NULL
    street.first<- NULL
    street.head.first <- NULL
    street.last<- NULL
    street.num<- NULL
    street.num.hi<- NULL
    street.num.lookup<- NULL
    street.num.low<- NULL
    street.num.range<- NULL
    street.second.last<- NULL
    street.type<- NULL
    street.unit<- NULL
    street.unit.type<- NULL
    l.regex <- clean.street.regex()
    # tic <- Sys.time()
    # street <- sapply(street, function(x) clean.street(x, l.regex))
    # toc <- Sys.time()
    # difftime(toc, tic, 'mins')
    tic <- Sys.time()
    n.cores <- detectCores() - 2
    n.cores <- max(n.cores, 1)
    street <- unlist(mclapply(street, function(y) clean.street(y, l.regex), mc.cores = n.cores))
    toc <- Sys.time()
    difftime(toc, tic, 'mins')

    street.explode <- data.table(street=street, street.num = '', street.direction.prefix = '', street.type = '',
                                 street.unit = '', street.unit.type = '', street.direction.suffix='',
                                 street.num.low = '', street.num.hi = '', street.last = '', street.second.last = '')

    # Initialize direction abbrev matches
    abbrev.direction <- methods.string::abbrev[class=='compass']
    search.direction <- abbrev.direction$search.regex
    return.direction <- abbrev.direction$return
    names(return.direction) <- search.direction

    # Prefix: street.num
    street.explode <- street.explode[, street.cum := street]
    street.explode <- street.explode[, street.head.first := str_extract(street.cum, regex('(?=^)[(0-9)|-]{1,}\\s', perl=TRUE))] # Debug 7, 8
    street.explode <- street.explode[is.na(street.head.first), street.head.first:='']
    street.explode <- street.explode[, street.num:=street.head.first]
    street.explode <- street.explode[street.head.first!='', street.cum:=str_trim(str_replace(street.cum, street.head.first, ''))]
    street.explode <- street.explode[, street.head.first := '']
    street.explode <- street.explode[, street.first := str_trim(lapply(str_split(street, ' '), '[[', 1))]
    street.explode <- street.explode[is.na(street.num), street.num:='9999999']

    ## Prefix: street.num.low, street.num.high
    street.explode <- street.explode[, street.num.low := str_trim(lapply(str_split(street.first, '-'), '[[', 1))]
    street.explode <- street.explode[, street.num.low := str_extract(street.num.low, regex('[0-9]{1,}(?=[^0-9]|$)', perl=TRUE))]
    street.explode <- street.explode[, street.num.hi := str_trim(lapply(str_split(street.first, '-'), '[', 2))]
    street.explode <- street.explode[, street.num.hi := str_extract(street.num.hi, regex('[0-9]{1,}(?=[^0-9]|$)', perl=TRUE))]
    street.explode <- street.explode[is.na(street.num.low), street.num.low := '']
    street.explode <- street.explode[is.na(street.num.hi), street.num.hi := street.num.low]
    street.explode$street.first <- NULL
    street.explode$street.num <- NULL

    # Prefix: street.direction.prefix
    direction.set <- abbrev.direction[, search]
    street.explode <- street.explode[, street.head.first := str_trim(lapply(str_split(street.cum, ' '), '[[', 1))]
    street.explode <- street.explode[street.head.first %in% direction.set, street.direction.prefix := street.head.first] # Debug 3 fixed here

    street.explode <- street.explode[street.head.first %in% direction.set, street.cum :=
                                         str_trim(str_replace(street.cum,
                                                              regex(paste0('(?=^)', street.head.first, '\\s'), perl=TRUE), ''))]
    street.explode <- street.explode[street.head.first %in% direction.set, street.direction.prefix :=
                                         str_trim(str_replace_all(street.direction.prefix, return.direction))]
    street.explode$street.head.first <- NULL

    # Suffix
    ## Initialize street.last, street.second.last
    street.cum.word.count <- word.count(street.explode$street.cum)
    update.log <- street.cum.word.count > 1
    street.explode <- parcels.street.last(update.log, street.explode)
    update.log <- street.cum.word.count > 2
    street.explode <- parcels.street.second.last(update.log, street.explode)
    street.cum.word.count <- word.count(street.explode$street.cum)
    '3280 Boulder Cir Bldg G4 West'
    search.set <- c(methods.string::abbrev[class=='unit'|class=='suffix', search],'')
    update.log <- !(street.explode$street.last %in% search.set) & !(street.explode$street.second.last %in% search.set)
    street.explode <- street.explode[update.log, street.last := paste(street.second.last, street.last)]
    street.explode <- parcels.street.second.last(update.log, street.explode)
    street.cum.word.count <- word.count(street.explode$street.cum)

    ## Step 0.0: First swipe of  street.unit (no street.type)
    ### Debug 9, 10, 11, 13, 14
    street.explode <- street.explode[str_detect(street.last, regex('(?<=(^|-))[0-9]{1,}(?=$)', perl=TRUE)) &
                                         !(street.second.last %in% methods.string::abbrev[class=='unit', search]),
                                     street.unit:=street.last]
    ### Debug 15 '6390 Gunpark Dr B'
    x <- street.explode
    c1 <- !(x$street.second.last %in% methods.string::abbrev[class=='unit', search])
    c2 <- !(x$street.last %in% methods.string::abbrev[class=='suffix', search])
    c3 <- !(x$street.last %in% methods.string::abbrev[class=='compass', search])
    c4 <- x$street.last !=''
    c5 <- str_detect(x$street.last, regex('[A-z]{3,}$', perl=TRUE))==FALSE  #"4845 Van Gordon
    logical <- cbind(c1,c2,c3,c4,c5)
    logical <-apply(logical, 1, function(x) length(x[which(x==TRUE)])==length(x))
    street.explode<-street.explode[logical, street.unit:=street.last]
    update.log <- street.explode$street.unit != ''
    street.explode <- parcels.street.last.promote(update.log, street.explode)
    street.explode <- parcels.street.second.last(update.log, street.explode)
    #"3201 W Hampden Ave Ste"
    #update.promote <- street.explode$street.last == '' & street.explode$street.second.last != ''
    #street.explode <- street.explode[update.promote, street.last := street.second.last]
    #street.explode <- street.explode[update.promote, street.second.last := '']
    #street.explode <- parcels.street.second.last(update.log, street.explode)

    ## Step 0.5: First swipe of  Assign street.direction.suffix (Debug 4)
    street.explode <- parcels.street.direction.suffix(street.explode, return.direction, abbrev.direction)

    ## Step 1: Revisit street.unit, street.unit.type
    regex.unit <- paste0(paste(methods.string::abbrev[class=='unit', return], collapse = '|'))
    unit.set <- methods.string::abbrev[class=='unit', search]
    street.unit.base <- street.explode$street.unit
    update.log <- street.explode$street.second.last %in% unit.set & street.unit.base==''
    street.explode <- street.explode[update.log, street.unit:= street.last]

    search.unit.type <- paste0('(?<=^)',unit.set,'(?=$)')
    return.unit.type <- methods.string::abbrev[class=='unit', return]
    names(return.unit.type) <- search.unit.type

    street.explode <- street.explode[update.log,
                                     street.unit.type := str_replace_all(street.second.last, return.unit.type)]
    street.explode <- parcels.street.last(update.log, street.explode)
    street.explode <- parcels.street.second.last(update.log, street.explode)

    # Step 2: Revisit Assign street.direction.suffix
    street.direction.suffix.base <- street.explode$street.direction.suffix
    street.explode <- parcels.street.direction.suffix(street.explode, return.direction, abbrev.direction)
    update.log <- street.explode$street.direction.suffix != '' & street.direction.suffix.base == ''  # Debug 5/6
    street.explode <- parcels.street.last(update.log, street.explode)
    street.explode <- parcels.street.second.last(update.log, street.explode)

    # Step 3 Assign street.type
    ## Speed up
    street.types <- unique(street.explode$street.last)
    abbrev.street.type <- methods.string::abbrev[class=='suffix' & search %in% street.types]
    # Debug 16
    #if (nrow(abbrev.street.type) >0){
    search.regex.street.type <- paste0('(?i)(?<=^)(',abbrev.street.type$search,')(?=$)')
    regex.street.type <- paste0('(?i)(?<=^)(',paste0(abbrev.street.type$search,collapse='|'),')(?=$)')
    return.street.type <- abbrev.street.type$return
    if (length(return.street.type)>0){
        names(return.street.type) <- search.regex.street.type
        street.explode <- street.explode[str_detect(street.last, regex(regex.street.type, perl=TRUE)),
                                         street.type := str_replace_all(street.last,return.street.type)]
    }
    update.log <- street.explode$street.type != ''  # Fixed Debug 2
    street.explode <- parcels.street.last.promote(update.log, street.explode)
    street.explode <- parcels.street.second.last(update.log, street.explode)
    #}
    # street.body (create and clean)
    vec <- c('rd','nd','th', 'st')
    matches.pattern <- paste0('(?i)(?<=(^| )[0-9]{1,10})(', vec, ')(?= |$)')
    matches.replacement <- vec
    names(matches.replacement) <- matches.pattern

    street.explode <- street.explode[, street.body:=str_trim(paste(street.cum, street.second.last, street.last))]
    street.explode <- street.explode[, street.body := str_replace_all(street.body, matches.replacement)]
    street.explode <- street.explode[, street.body := str_replace_all(street.body, '[\\s]{1,}', ' ')]

    # Error catch for PO Box
    inds.pi <- str_detect(street, regex('(?i)Box [0-9]{1,}(?=$)'))
    street.explode <- street.explode[inds.pi==TRUE, street:=street]
    street.explode <- street.explode[inds.pi==TRUE, street.num.low:='0']
    street.explode <- street.explode[inds.pi==TRUE, street.num.hi:='0']
    street.explode <- street.explode[inds.pi==TRUE, street.direction.prefix:='']
    street.explode <- street.explode[inds.pi==TRUE, street.direction.suffix:='']
    street.explode <- street.explode[inds.pi==TRUE, street.unit:=str_extract(street, regex('[0-9]{1,}(?=$)',perl=TRUE))]
    street.explode <- street.explode[inds.pi==TRUE, street.unit.type:='Box']
    street.explode <- street.explode[inds.pi==TRUE, street.body:='PO']

    street.explode$street.last <- NULL
    street.explode$street.second.last <- NULL
    street.explode$street.cum <- NULL

    street.explode <- street.explode[, street.num.hi:=as.integer(street.num.hi)]
    street.explode <- street.explode[, street.num.low:=as.integer(street.num.low)]
    street.explode$street.num <- street.explode$street.num.low

    return(street.explode)
}
#' @title extract.state
#'
#' @description Extracts state from cityStateZip.
#' @param cityStateZip character vector with cityStateZip
#' @keywords extract, state
#' @export
#' @import stringr
extract.state = function(cityStateZip){
    cityStateZip <- clean.state(cityStateZip)
    regex.state.abb <- paste0('(',paste0(methods.string::lookup.state$abb, collapse='|'), ')')
    state <- str_extract(cityStateZip, regex(regex.state.abb, perl=TRUE))
    return(state)
}
#' @title extract.zip
#'
#' @description Extracts zipcode from cityStateZip.
#' @param cityStateZip charcater vector with cityStateZip
#' @keywords extract, zipcode, zip
#' @export
#' @import stringr
extract.zip = function(cityStateZip){
    zip <- str_extract(cityStateZip, regex('(?<=( |^|,|\\w))[0-9]{5,5}', perl=TRUE))
    zip[is.na(zip)] <- ''
    return(zip)
}
#' @title fill.missing.state
#'
#' @description Fills missing state by querying zips
#' @param x data.table consisting of address.id and zip
#' @keywords fill missing, zip, city, state
#' @export
#' @import stringr
#'     data.table
#' @importFrom dplyr mutate select one_of group_by_
fill.missing.state <- function(x){
    if('address.id' %in% names(x)){
    state <- NULL
    address.id <- NULL
    zip <- NULL
    zip.find <- NULL
    dt <- x[, .(zip.find=zip, address.id)]
    setkey(dt, zip.find)
    setkey(methods.string::lookup.zips,zip)
    dt <- methods.string::lookup.zips[dt]
    # county by state and fill
    states.found <- table(dt$state)
    states.found <- states.found[str_length(names(states.found))==2]
    states.found.n <- length(states.found)
    states.found.pct <- states.found/sum(states.found)
    states.notfound.pct <- states.found/nrow(dt)
    state.fill <- names(states.found)[which.max(states.found)]
    if(max(states.found.pct) > 0.9 & max(states.notfound.pct)){
       state.filled.n <- nrow(dt[zip==''])
       dt <- dt[zip=='', state:=state.fill]
       cat('Filled', state.filled.n, 'state records with', state.fill,'based on study consistency')
    }
    dt <- dt[, .(address.id, state)]
    setkey(dt, address.id)
    return(dt)
    } else {
        print('No address.id in methods.string::fill.missing.state. Omitted function')
        return(x)
    }
}
#' @title fill.missing.zip.city
#'
#' @description Fills missing zip and city by looking up in lookup.address if it exists.
#' @param DT data.table with exploded zip city state
#' @param lookup.address data.table used for lookup values
#' @keywords fill missing, zip, city, state
#' @export
#' @import stringr
#'     data.table
#'     dplyr
fill.missing.zip.city <- function(DT, lookup.address=NULL){
    city <- NULL
    street.num <- NULL
    num.distance <- NULL
    street.num.lookup <- NULL
    print('Attempting to fill missing zip and cities')
    # Convert NA values to ''f
    DT <- DT[is.na(zip), zip:='']
    DT <- DT[is.na(city), city:='']
    DT <- DT[str_detect(city, regex('(?i)(?<=( |^))na(?=( |$))', perl=TRUE)), city:='']
    DT.colnames <- names(DT)
    lookup.var <- 'zip'
    check.id <- grepl('.id', paste0(DT.colnames,collapse=','))
    if(check.id==FALSE){
        DT$temp.id <- seq(1:nrow(DT))
        var.id <- 'temp.id'
    } else {
        var.id <- grep('.id', DT.colnames,value = TRUE)
    }
    val.id <- as.symbol(var.id)

    vars.list <- list()
    vars.list$key.vars[[1]] <- c('street.direction.prefix', 'street.body', 'street.type', 'city', 'street.num')
    vars.list$lookup.var[[1]] <- 'zip'
    vars.list$key.vars[[2]] <- c('street.direction.prefix', 'street.body', 'street.type', 'street.num')
    vars.list$lookup.var[[2]] <- 'zip'
    vars.list$key.vars[[3]] <- c('street.direction.prefix', 'street.body', 'street.type', 'zip', 'street.num')
    vars.list$lookup.var[[3]] <- 'city'
    vars.list$key.vars[[4]] <- c('street.direction.prefix', 'street.body', 'street.type', 'street.num')
    vars.list$lookup.var[[4]] <- 'city'

    # Used for join (drop zip from colname)
    for(iComb in 1:length(vars.list$key.vars)){
        #print(iComb)
        # Set variable parameters
        key.vars <- vars.list$key.vars[[iComb]]
        lookup.var <- vars.list$lookup.var[[iComb]]
        lookup.val <- as.symbol(lookup.var)
        #DT.full.vars <- grep(lookup.var, DT.colnames, invert=TRUE, value=TRUE)
        #baseVars <- c(var.id, key.vars)

        # Initialize DT tables
        #DT.good <- DT[eval(lookup.val)!='']
        DT.missing <- DT[eval(lookup.val)=='']
        n.missing <- nrow(DT.missing)
        if(n.missing>0){
            if(exists('DT.lookup')){
                rm(DT.lookup)
            }
            DT.lookup <- DT.missing[, street.num := as.numeric(street.num)] #Not sure about this
            full.vars <- c(lookup.var, key.vars)
            #key.vals <- lapply(key.vars, as.symbol)
            lookup.val <- as.symbol(lookup.var)
            #full.vals <- lapply(full.vars, as.symbol)
            #vars.lookup <- c(key.vars, 'val.new')
            val.id <- as.symbol(var.id)

            if (exists('lookup.address')){
                lookup.address.table <- lookup.address[city!='' & zip != ''] %>%
                    dplyr::select(one_of(full.vars)) %>%
                    dplyr::mutate(street.num = as.numeric(street.num)) %>%
                    dplyr::mutate(street.num.lookup = street.num)
                lookup.address.table <- as.data.table(lookup.address.table)
            } else {
                lookup.address.table <- DT[city!='' & zip != ''] %>%
                    dplyr::select(one_of(full.vars)) %>%
                    dplyr::mutate(street.num = as.numeric(street.num)) %>%
                    dplyr::mutate(street.num.lookup = street.num)
                lookup.address.table <- as.data.table(lookup.address.table)
            }
            setkeyv(DT.lookup, key.vars)
            setkeyv(lookup.address.table, key.vars)
            DT.lookup.back <- lookup.address.table[DT.lookup, roll=-Inf]
            DT.lookup.forward <- lookup.address.table[DT.lookup, roll=Inf]
            DT.lookup <- rbindlist(list(DT.lookup.back, DT.lookup.forward))
            DT.lookup <- DT.lookup[, num.distance:=abs(street.num-street.num.lookup)]

            DT.lookup <-   DT.lookup %>%
                dplyr::group_by_(.dots=list(val.id)) %>%
                filter(rank(-num.distance, ties.method='first')==1)
            DT.lookup <- as.data.table(DT.lookup)
            DT.lookup <- DT.lookup[!is.na(street.num)]
            DT.lookup <- dplyr::select(DT.lookup, one_of(names(DT.missing)))
            DT.fixed <- DT.lookup[!is.na(eval(lookup.val))]

            checks <- vars.list$key.vars[[iComb]]
            bigs <- list('city', 'zip')
            if(length(unlist(sapply(bigs, function(x) which(x %in% checks))))==0){
                val.check <- as.symbol(bigs[[which(!(bigs %in% lookup.var))]])
                bad.ids <- unlist(DT[eval(val.check)!=''][, (var.id), with=FALSE])
                DT.fixed<-DT.fixed[!(eval(val.id) %in% bad.ids)]
            }

            if (nrow(DT.fixed)>0){
                # Fixed DT.id
                ids.fixed <- unlist(DT.fixed[, get(var.id)])
                ids.full <- unlist(DT[, get(var.id)])
                ids.keep <- which(!(ids.full %in% ids.fixed))
                DT <- DT[ids.keep]
                DT <- rbindlist(list(DT, DT.fixed), use.names=TRUE)
            }
            print(paste0(paste0(lookup.var, collapse=','),
                         ': missing ', nrow(DT.missing),': fixed ', nrow(DT.fixed)))
        }
    }
    if(check.id==FALSE) DT$temp.id<-NULL
    return(DT)
}
#' @title parcels.street.last
#'
#' @description Suffix operations for explode.street
#' @param update.log logical vector
#' @param street.explode data.table
#' @keywords street.explode, explode, helper
#' @export
#' @import stringr
#'     data.table
parcels.street.last <- function(update.log, street.explode){
    street.last <- NULL
    street.cum <- NULL
    street.explode <- street.explode[update.log, street.last := str_trim(str_replace(street.cum, regex('.*. ', perl=TRUE), ''))]
    street.explode <- street.explode[update.log,
                                     street.cum := str_trim(str_replace(street.cum, regex(paste0('(^|\\s)',street.last,'(?=$)'),
                                                                                          perl=TRUE), ''))]
    return(street.explode)
    }
#' @title parcels.street.last.promote
#'
#' @description Helper function for explode.street.
#' @param update.log logical vector
#' @param street.explode data.table
#' @keywords street.explode, explode, helper
#' @export
#' @import stringr
#'    data.table
parcels.street.last.promote <- function(update.log, street.explode){
    street.second.last <- NULL
    street.last <- NULL
    street.explode <- street.explode[update.log, street.last := street.second.last]
}
#' @title parcels.street.second.last
#'
#' @description Helper function for explode.street.
#' @param update.log logical vector
#' @param street.explode data.table
#' @keywords street.explode, explode, helper
#' @export
#' @import stringr
#'     data.table
parcels.street.second.last <- function(update.log, street.explode){
    street.cum <- NULL
    street.second.last <- NULL
    street.explode <- street.explode[update.log, street.second.last := str_trim(str_replace(street.cum, regex('.* ', perl=TRUE), ''))]
    street.explode <- street.explode[update.log, street.cum:=str_trim(str_replace(street.cum, regex(paste0('(^|\\s)',street.second.last,'(?=$)'), perl=TRUE), ' '))]
}
#' @title parcels.street.direction.suffix
#'
#' @description Helper function for explode.street.
#' @param street.explode data.table
#' @param abbrev.direction list of abbreviations to search for
#' @param return.direction no idea
#' @keywords street.explode, explode, helper
#' @export
#' @import stringr
#'    data.table
parcels.street.direction.suffix <- function(street.explode, return.direction, abbrev.direction){
    street.direction.suffix <- NULL
    street.last <- NULL
    street.num<-NULL
    street.num.lookup<-NULL
    #regex.direction <- paste0('(?i)(?<=^| )(', paste(abbrev.direction$search, collapse='|'), ')(?=$)')
    direction.set <- abbrev.direction$search
    update.log <- street.explode$street.last %in% direction.set
    street.explode <- street.explode[update.log,
                                     street.direction.suffix := street.last]
    street.explode <- street.explode[update.log,
                                     street.direction.suffix := str_trim(str_replace_all(street.direction.suffix,
                                                                                         return.direction))]
    street.explode <- parcels.street.last.promote(update.log, street.explode)
    street.explode <- parcels.street.second.last(update.log, street.explode)
    return(street.explode)
}
#' @title word.count
#'
#' @description Helper function for explode.street, sees how may words are in the holder street.cum, also binds some words together.
#' @param street.cum character vector
#' @keywords street.explode, explode, helper, cound words
#' @export
#' @import stringr
word.count <- function(street.cum){
    word.count <- sapply(strsplit(street.cum, "\\s+"), length)
    notting <- str_detect(street.cum, regex(paste0('Notting Hill '), perl=TRUE))
    july <- str_detect(street.cum, regex(paste0('Fourth Of July '), perl=TRUE))
    county.road <- str_detect(street.cum, regex(paste0('County (Road|Rd) ', '[0-9]{1,}(?=\\s|$)'), perl=TRUE))
    highway <- str_detect(street.cum, regex(paste0('(Highway |Hwy )', '[0-9]{1,}(?=\\s|$)'), perl=TRUE))
    us.highway <- str_detect(street.cum, regex(paste0('US (Highway|Hwy) ', '[0-9]{1,}(?=\\s|$)'), perl=TRUE))
    state.highway <- str_detect(street.cum, regex(paste0('State (Highway|Hwy) ', '[0-9]{1,}(?=\\s|$)'), perl=TRUE))
    foothills.highway <- str_detect(street.cum, regex(paste0('Foothills Hwy ', '[0-9|A-z]{1,}(?=\\s|$)'), perl=TRUE))
    peak2peak.highway <- str_detect(street.cum, regex(paste0('Peak To Peak Hwy ', '[0-9]{1,}(?=\\s|$)'), perl=TRUE))
    peak2peak <- str_detect(street.cum, regex(paste0('(Peak To Peak |Paseo Del Prado )', '[0-9]{1,}(?=\\s|$)'), perl=TRUE))
    word.count[notting] <- word.count[notting] - 2
    word.count[july] <- word.count[july] - 3
    word.count[county.road] <- word.count[county.road] - 2
    word.count[highway] <- word.count[highway] - 1
    word.count[us.highway] <- word.count[us.highway] - 2
    word.count[state.highway] <- word.count[state.highway] - 2
    word.count[foothills.highway] <- word.count[foothills.highway] - 1
    word.count[peak2peak.highway] <- word.count[peak2peak.highway] - 3
    word.count[peak2peak] <- word.count[peak2peak] - 2
    return(word.count)
}
#' @title underscore
#'
#' @description Convert spaces, dashes, and antying else to underscore.
#' @param x character vector
#' @keywords street.explode, explode, helper, cound words
#' @export
underscore <- function(x){
    x <- tolower(x)
    x <- gsub("(_.)", "\\U\\1", x, perl = TRUE)
    x <- gsub("(-.)", "\\U\\1", x, perl = TRUE)
    x <- gsub("( .)", "\\U\\1", x, perl = TRUE)
    x <- gsub(",", ' ', x, perl = TRUE)
    x <- gsub(" ", '-', x, perl = TRUE)
    x <- gsub("--", '-', x, perl = TRUE)
    x <- gsub("--", '-', x, perl = TRUE)
    x <- gsub("--", '-', x, perl = TRUE)
    x <- gsub('_', '', x, perl = TRUE)
    x <- gsub('-', '', x, perl = TRUE)
}






