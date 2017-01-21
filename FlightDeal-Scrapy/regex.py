    
# Test cases
"Valid for travel in early December or January, 2017."
"Valid for travel on the outbound until early December or January, 2017 - March, 2017."
"Valid for travel on the outbound until early December or January, 2017 - early April, 2017."
"Valid for travel Novemeber 30th - December 3rd."
"Valid for travel until January, 2017."
"Valid for travel on the outbound from late November - early December or January, 2017 - early May, 2017."
"Valid for travel mid October - mid December."
"Valid for travel November 10th - 30th" # this one was cold... one ? at the space after year

    def date_extractor(self, post_date, date_string):
        '''Extract month and year from a sentence. These are pretty case specific'''
        # "from Month, Year - Month, Year"
        # split start time, end time by dash
        # split by comma to see year
        # otherwise add year from date

        # from mid/late? Month Day?, Year? - mid/early? Month? Day?, Year?
        # ex. from mid January 30th, 2017 - August 15th, 2017
        # from January 1st - 30th
        # from January - early August
        # from January 1st to August 30th 
        from_to = re.findall('(?<=from )((mid |late )?(January|February|March|April|May|June|July|August|September|October|November|December)( \d+(th|st|nd|rd))?(, \d{4})? - (early |mid )?(January|February|March|April|May|June|July|August|September|October|November|December)?( ?\d+(th|st|nd|rd))?(, \d{4})?)', \
        date_string)

        or_to = re.findall('(?<=or )((mid |late )?(January|February|March|April|May|June|July|August|September|October|November|December)( \d+(th|st|nd|rd))?(, \d{4})? - (early |mid )?(January|February|March|April|May|June|July|August|September|October|November|December)?( \d+(th|st|nd|rd))?(, \d{4})?)', \
        date_string)

        until = re.findall('(?<=until )((early |mid |late )?(January|February|March|April|May|June|July|August|September|October|November|December)( \d+(th|st|nd|rd))?(, \d{4})?)', \
        date_string)

        in_start = re.findall('(?<=in )((early |mid |late )?(January|February|March|April|May|June|July|August|September|October|November|December)( \d+(th|st|nd|rd))?(, \d{4})?)', \
        date_string)

        # hopefully there aren't some really weird cases with comma
        # basically when multiple time frames are listed
        # i.e. January or March - June, August - September.
        comma_to = re.findall('(?<=, )((mid |late )?(January|February|March|April|May|June|July|August|September|October|November|December)( \d+(th|st|nd|rd))?(, \d{4})? - (early |mid )?(January|February|March|April|May|June|July|August|September|October|November|December)?( \d+(th|st|nd|rd))?(, \d{4})?)', \
        date_string)





        # swap month and day to mm/dd/year
        post_date_split = self.u2s(post_date).split(' ')
        post_date_split[0], post_date_split[1] = post_date_split[1], post_date_split[0]


        parsed_dates = ''


        if until != []:
            parsed_dates += '/' + ' '.join(post_date_split) + ' ' + '^'.join([g[0] for g in until])

        if in_start != []:
            parsed_dates += '/' + '^'.join([g[0] for g in in_start])

        if from_to != []:
            parsed_dates += '/' + '^'.join([g[0] for g in from_to])

        if or_to != []:
            parsed_dates += '/' + '^'.join([g[0] for g in or_to])

        if comma_to != []:
            parsed_dates += '/' + '^'.join([g[0] for g in comma_to])            

        if parsed_dates == '':


            parsed_dates = 'Not Found'

        return parsed_dates

    def miles_extractor(self, miles_string):

        miles = re.search('\d+(,\d+)?(?= miles)', miles_string).group(0)
        miles = re.sub(',', '', miles)

        return miles