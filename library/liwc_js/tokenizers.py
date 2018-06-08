#!/usr/bin/env python
# -*- coding: utf-8 -*-

import re
import htmlentitydefs
from collections import defaultdict
import dateutil.parser
import sys
import signal

######################################################################

# HTML entities:
html_entity_digit_re = re.compile(r"&#\d+;")
html_entity_alpha_re = re.compile(r"&\w+;")
amp = "&amp;"

def html2unicode(s):
    """
    Seeks to replace all the HTML entities in s with their
    corresponding unicode characters.
    """
    # First the digits:
    ents = set(html_entity_digit_re.findall(s))
    if len(ents) > 0:
        for ent in ents:
            entnum = ent[2:-1]
            try:
                entnum = int(entnum)
                s = s.replace(ent, unichr(entnum))	
            except:
                pass
    # Now the alpha versions:
    ents = set(html_entity_alpha_re.findall(s))
    ents = filter((lambda x : x != amp), ents)
    for ent in ents:
        entname = ent[1:-1]
        try:            
            s = s.replace(ent, unichr(htmlentitydefs.name2codepoint[entname]))
        except:
            pass                    
    s = s.replace(amp, " and ")
    return s

######################################################################

tags = r"""<[^>]+>"""    

######################################################################

phone_numbers = r"""
  (?:
    (?:            # (international)
      \+?[01]
      [\-\s.]*
    )?            
    (?:            # (area code)
      [\(]?
      \d{3}
      [\-\s.\)]*
    )?    
    \d{3}          # exchange
    [\-\s.]*   
    \d{4}          # base
  )"""

######################################################################

prices = "[\$₤£¥€]\d+[\d.,]+\d+"

######################################################################

urls = r"""
  (?:                           
    (?:
      [a-z][\w-]+:                            # URL protocol and colon
      (?:
        /{1,3}                                # 1-3 slashes
        |                                     # or
        [a-z0-9%]                             # Single letter or digit or '%' (Trying not to match e.g. "URI::Escape")
      )
      |                                       #  or
      www\d{0,3}[.]                           # "www.", "www1.", "www2." … "www999."
      |                                       #   or
      [a-z0-9.\-]+[.][a-z]{2,4}/              # looks like domain name followed by a slash
    )
    (?:                                       # One or more:
      [^\s()<>]+                              # Run of non-space, non-()<>
      |                                       #   or
      \((?:[^\s()<>]+|(?:\([^\s()<>]+\)))*\)  # balanced parens, up to 2 levels
    )+
    (?:                                       # End with:
      \((?:[^\s()<>]+|(?:\([^\s()<>]+\)))*\)  # balanced parens, up to 2 levels
      |                                       # or
      [^\s`!()\[\]{};:'".,<>?«»“”‘’]           # not a space or one of these punct chars
     )
  )"""

######################################################################

long_dates = r"""
    (?: 
      (?:0?[1-9]|1[12])      # month
      [\-\s/.\\]+
      (?:[012]?[0-9]|3[01])  # day
      [\-\s/.\\]+
      \d{2,4}                # year
      |
      (?:[012]?[0-9]|3[01])  # day
      [\-\s/.\\]+
      (?:0?[1-9]|1[12])      # month
      [\-\s/.\\]+
      \d{2,4}                # year
      |
      \d{2,4}                # year
      [\-\s/.\\]+
      (?:0?[1-9]|1[12])      # month
      [\-\s/.\\]+
      (?:[012]?[0-9]|3[01])  # day
      | 
      (?:[012]?[0-9]|3[01])
      \s+
      (?:
        (?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Oct|Nov|Dec)\.?
        |
        (?:January|February|March|April|May|June|July|August|September|October|November|December)
      )
      \s+
      \d{2,4}
      | 
      (?:
        (?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Oct|Nov|Dec)\.?
        |
        (?:January|February|March|April|May|June|July|August|September|October|November|December)
      )
      [\s,]+
      (?:[012]?[0-9]|3[01])
      [\s,]+
      \d{2,4}
   )"""

short_dates = r"""
  (?:
    (?:
      (?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Oct|Nov|Dec)\.?
      |
      (?:January|February|March|April|May|June|July|August|September|October|November|December)
    )
    \s+
    (?:[012]?[0-9]|3[01])
    |
    (?:[012]?[0-9]|3[01])
    \s+
    (?:
      (?:Jan|Feb|Mar|Apr|May|Jun|Jul|Aug|Sept?|Oct|Nov|Dec)\.?
      |
      (?:January|February|March|April|May|June|July|August|September|October|November|December)
    )
  )"""

######################################################################

emoticons = r"""
  (?:                           # non-capturing group
    [<>]?                       # optional hat/brow
    [:;=8]                      # eyes
    [\-o\*\']?                  # optional nose
    [\)\]\(\[dDpP/\:\}\{@\|\\]  # mouth      
    |                           #### reverse orientation
    [\)\]\(\[dDpP/\:\}\{@\|\\]  # mouth
    [\-o\*\']?                  # optional nose
    [:;=8]                      # eyes
    [<>]?                       # optional hat/brow
  )"""

######################################################################

hat_tip = r"[Hh]/[Tt]"

email = r"(?:[\w._%+-]+@[\w._%+-]+\.\w{2,})"

twitter_username = r"""(?:@[\w_]+)"""

twitter_hashtag = r"(?:\#+[\w_]+[\w\'_\-]*[\w_]+)"

######################################################################

words = r"""
  (?:[a-z][a-z'\-_]+[a-z])       # Words with apostrophes or dashes.
  |
  (?:[+\-]?\d+[,/.:-]\d+[+\-]?)  # Numbers, including fractions, decimals.
  |
  (?:[\w_]+)                     # Words without apostrophes or dashes.
  |
  (?:\.(?:\s*\.){1,})            # Ellipsis dots.
  |
  (?:\*{1,})                     # Asterisk runs.
  |
  (?:\S)                         # Everything else that isn't whitespace.
  """                 

######################################################################
# POOL THE ABOVE STRINGS INTO A SINGLE ORDERED REGULAR EXPRESSION:

regex_strings = (tags, phone_numbers, prices, urls, long_dates, short_dates, emoticons, hat_tip, email, twitter_username, twitter_hashtag, words)
    
word_re = re.compile(r"""(%s)""" % "|".join(regex_strings), re.VERBOSE | re.I | re.UNICODE)

######################################################################
# The following regexs facilitate various filtering functions for
# tokenizing.  They are meant to be applied AFTER the string has been
# chunked into words.

html_tag_re = re.compile(tags, re.UNICODE)

emoticon_re = re.compile(emoticons, re.VERBOSE | re.I) # Used to retain emoticon case.

twitter_usename_re = re.compile(r"^@") # Used to remove usernames.

twitter_hashtag_re = re.compile(r"^#") # Used to remove hashtags.

url_re = re.compile(urls, re.VERBOSE)

long_date_re = re.compile(long_dates, re.VERBOSE | re.I)
short_date_re = re.compile(short_dates, re.VERBOSE | re.I)

negation_regex = re.compile(r"""
  (?:
    (?:
      ^(?:never|no|nothing|nowhere|noone|none|not|
          havent|hasnt|hadnt|cant|couldnt|shouldnt|wont|wouldnt|dont|doesnt|didnt|isnt|arent|aint
       )$
    )
    |
    n't
  )""", re.I | re.VERBOSE)

nonveridical_operators = (   
    'accept', 'accepts', 'accepted', 'accepting',
    'advertise', 'advertises', 'advertised', 'advertising',
    'advocate', 'advocates', 'advocated', 'advocating',
    'affirm', 'affirms', 'affirmed', 'affirming',
    'agree', 'agrees', 'agreed', 'agreeing',
    'allege', 'alleges', 'alleged', 'alleging',
    'allow', 'allows', 'allowed', 'allowing',
    'annoy', 'annoys', 'annoyed', 'annoying',
    'appear', 'appears', 'appeared', 'appearing', 
    'argue', 'argues', 'argued', 'arguing',
    'ask', 'asks', 'asked', 'asking',
    'assert', 'asserts', 'asserted', 'asserting',
    'assume', 'assumes', 'assumed', 'assuming',
    'assure', 'assures', 'assured', 'assuring',
    'believe', 'believes', 'believed', 'believing',
    'boast', 'boasts', 'boasted', 'boasting',
    'calculate', 'calculates', 'calculated', 'calculating',
    'claim', 'claims', 'claimed', 'claiming',
    'comment', 'comments', 'commented', 'commenting',
    'concern', 'concerns', 'concerned', 'concerning',
    'confided', 'confides', 'confided', 'confiding',
    'conjecture', 'conjectures', 'conjectured', 'conjecturing',
    'consider', 'considers', 'considered', 'considering',
    'contemplate', 'contemplates', 'contemplated', 'contemplating', 
    'decided', 'decides', 'decided', 'deciding',
    'declare', 'declares', 'declared', 'declaring',
    'deduce', 'deduces', 'deduced', 'deducing',
    'deem', 'deems', 'deemed', 'deeming',
    'demand', 'demands', 'demanded', 'demanding',
    'desire', 'desires', 'desired', 'desiring',
    'determine', 'determines', 'determined', 'determining',
    'doubt', 'doubts', 'doubted', 'doubting',
    'exclaim', 'exclaims', 'exclaimed', 'exclaiming',
    'expected', 'expects', 'expected', 'expecting',
    'fantasize', 'fantasizes', 'fantasized', 'fantasizing',
    'fear', 'fears', 'feared', 'fearing',
    'feel', 'feels', 'felt', 'feeling',
    'grumble', 'grumbles', 'grumbled', 'grumbling',
    'guess', 'guesses', 'guessed', 'guessing',
    'hear', 'hears', 'heard', 'hearing',
    'hope', 'hopes', 'hoped', 'hoping',
    'imply', 'implies', 'implied', 'implying',
    'imagine', 'imagines', 'imagined', 'imagining',
    'infer', 'infers', 'inferred', 'inferring',
    'insinuate', 'insinuates', 'insinuated', 'insinuating',
    'maintain', 'maintains', 'maintained', 'maintaining',
    'mumble', 'mumbles', 'mumbled', 'mumbling',
    'opine', 'opines', 'opined', 'opining',
    'pledge', 'pledges', 'pledged', 'pledging',
    'pray', 'prays', 'prayed', 'praying',
    'predict', 'predicts', 'predicted', 'predicting',
    'presume', 'presumes', 'presumed', 'presuming',
    'pretend', 'pretends', 'pretended', 'pretending',
    'proclaim', 'proclaims', 'proclaimed', 'proclaiming',
    'pronounce', 'pronounces', 'pronounced', 'pronouncing',
    'propose', 'proposes', 'proposed', 'proposing',
    'question', 'questions', 'questioned', 'questioning',
    'reckon', 'reckons', 'reckoned', 'reckoning',
    'recommend', 'recommends', 'recommended', 'recommending',
    'report', 'reports', 'reported', 'reporting',
    'respond', 'responds', 'responded', 'responding',
    'say', 'says', 'said', 'saying',
    'seem', 'seems', 'seemed', 'seeming',
    'shout', 'shouts', 'shouted', 'shouting',
    'suspect', 'suspects', 'suspected', 'suspecting',
    'think', 'thinks', 'thought', 'thinking',
    'tell', 'tells', 'told', 'telling',
    'want', 'wants', 'wanted', 'wanting',
    'wish', 'wishes', 'wished', 'wishing',
    'wonder', 'wonders', 'wondered', 'wondering'
    )

nonveridical_regex = re.compile(r"\b(%s)\b" % "|".join(nonveridical_operators))

quotation_regex = re.compile(r'"', re.UNICODE)

scope_close_re = re.compile(r"[.:;!?]", re.UNICODE)

all_caps_re = re.compile(r"^[A-Z\-\']+$", re.UNICODE)

elongation_re = re.compile(r"([a-zA-Z])\1{2,}")

nonsentiment_punctuation_re = re.compile(r"^[^a-zA-Z0-9!?]+$", re.UNICODE)

######################################################################

class TreebankTokenizer:
    # List of contractions adapted from Robert MacIntyre's tokenizer. 
    CONTRACTIONS2 = [re.compile(r"(?i)(.)('ll|'re|'ve|n't|'s|'m|'d)\b"), 
                    re.compile(r"(?i)\b(can)(not)\b"), 
                    re.compile(r"(?i)\b(D)('ye)\b"), 
                    re.compile(r"(?i)\b(Gim)(me)\b"), 
                    re.compile(r"(?i)\b(Gon)(na)\b"), 
                    re.compile(r"(?i)\b(Got)(ta)\b"), 
                    re.compile(r"(?i)\b(Lem)(me)\b"), 
                    re.compile(r"(?i)\b(Mor)('n)\b"), 
                    re.compile(r"(?i)\b(T)(is)\b"), 
                    re.compile(r"(?i)\b(T)(was)\b"), 
                    re.compile(r"(?i)\b(Wan)(na)\b")] 
    CONTRACTIONS3 = [re.compile(r"(?i)\b(Whad)(dd)(ya)\b"), 
                    re.compile(r"(?i)\b(Wha)(t)(cha)\b")] 

    def __init__(self, preserve_case=True):
        self.preserve_case = preserve_case   
       
    def tokenize(self, text):
        if not self.preserve_case:
            text = text.lower()
        for regexp in TreebankTokenizer.CONTRACTIONS2: 
            text = regexp.sub(r'\1 \2', text) 
        for regexp in TreebankTokenizer.CONTRACTIONS3: 
            text = regexp.sub(r'\1 \2 \3', text)    
        # Separate most punctuation 
        text = re.sub(r"([^\w\.\'\-\/,&])", r' \1 ', text)    
        # Separate commas if they're followed by space. 
        # (E.g., don't separate 2,500) 
        text = re.sub(r"(,\s)", r' \1', text)   
        # Separate single quotes if they're followed by a space. 
        text = re.sub(r"('\s)", r' \1', text)   
        # Separate periods that come before newline or end of string. 
        text = re.sub('\. *(\n|$)', ' . ', text)   
        return text.split() 

class NopunctTokenizer:
    def __init__(self, preserve_case=True):
        self.preserve_case = preserve_case
        
    def tokenize(self, s):
        if not self.preserve_case:
            s = s.lower()
        words = re.split(r"\s+", s)
        words =  map((lambda x : re.sub(r'\W', '', x)), words)
        return filter((lambda x : x), words)

class WhitespaceTokenizer:
    def __init__(self, preserve_case=True):
        self.preserve_case = preserve_case

    def tokenize(self, s):
        if not self.preserve_case:
            s = s.lower()
        return re.split(r"\s+", s)

######################################################################

class TimeoutException(Exception): 
    pass

def timeout_handler(signum, frame):
    raise TimeoutException()    

class TwitterTokenizer:
    def __init__(self,
                preserve_case=True,
                preserve_all_caps=True,
                filter_html_tags=False,
                filter_twitter_usernames=False,
                filter_twitter_hashtags=False,
                filter_urls=False,
                filter_dates=False,
                filter_nonsentiment_punctuation=False,
                mark_negation_scope=False,
                mark_quotation_scope=False,
                mark_nonveridical_scope=False,
                porter_stem=False,
                normalize_dates=False,
                normalize_elongations=False
                ):
        self.preserve_case = preserve_case
        self.preserve_all_caps = preserve_all_caps
        self.filter_html_tags = filter_html_tags
        self.filter_twitter_usernames = filter_twitter_usernames
        self.filter_twitter_hashtags = filter_twitter_hashtags
        self.filter_urls = filter_urls
        self.filter_dates = filter_dates
        self.filter_nonsentiment_punctuation = filter_nonsentiment_punctuation
        self.mark_negation_scope = mark_negation_scope
        self.mark_quotation_scope = mark_quotation_scope
        self.mark_nonveridical_scope = mark_nonveridical_scope
        self.porter_stem = porter_stem
        self.normalize_dates = normalize_dates
        self.normalize_elongations = normalize_elongations

    def tokenize(self, s):
        # Try to ensure unicode:
        try:
            s = unicode(s)
        except UnicodeDecodeError:
            s = str(s).encode('string_escape')
            s = unicode(s)
        # Fix HTML character entitites:
        s = html2unicode(s)

        old_handler = signal.signal(signal.SIGALRM, timeout_handler)
        secs = int(round(1 + (len(s)/200000.0), 0))
        signal.alarm(secs) # triger alarm in 3 seconds
        try:
            words = word_re.findall(s)
        except TimeoutException:
            return []
        finally:
            signal.signal(signal.SIGALRM, old_handler) 
            signal.alarm(0)
        # Case:
        if not self.preserve_case:
            if self.preserve_all_caps:
                words = map((lambda x : x if all_caps_re.search(x) or emoticon_re.search(x) else x.lower()), words)
            else:
                words = map((lambda x : x if emoticon_re.search(x) else x.lower()), words)
        # HTML tags
        if self.filter_html_tags:
            words = filter((lambda x : not html_tag_re.search(x)), words)
        # Twitter usernames:
        if self.filter_twitter_usernames:
            words = filter((lambda x : not twitter_usename_re.search(x)), words)
        # Twitter hashtags:
        if self.filter_twitter_hashtags:
            words = filter((lambda x : not twitter_hashtag_re.search(x)), words)
        # URLs:
        if self.filter_urls:
            words = filter((lambda x : not url_re.search(x)), words)
        # Dates:
        if self.normalize_dates:
           words = map(self.date_normalizer, words)
        if self.filter_dates:
           words = filter((lambda x : not long_date_re.search(x) and not short_date_re.search(x)), words)
        # Elongation:
        if self.normalize_elongations:
           words = map((lambda x : elongation_re.sub(r"\1\1\1", x)), words)
        # Porter stemming:
        if self.porter_stem:
            from nltk.stem.porter import PorterStemmer
            words = map((lambda x : PorterStemmer().stem(x)), words)
        # Negation:
        if self.mark_negation_scope:
            negating = False
            for i, word in enumerate(words):
                if negation_regex.search(word):
                    negating = True
                elif scope_close_re.search(word) or emoticon_re.search(word):
                    negating = False  
                elif negating:
                    words[i] = word + "_NEG"
        # Nonveridical:
        if self.mark_nonveridical_scope:
            intensional = False
            for i, word in enumerate(words):
                if nonveridical_regex.search(word):
                    intensional = True
                elif scope_close_re.search(word) or emoticon_re.search(word):
                    intensional = False  
                elif intensional and not word.endswith("_QUOTE"):
                    words[i] = word + "_QUOTE"
        # Quotation:
        if self.mark_quotation_scope:
            quoting = False
            for i, word in enumerate(words):
                if quoting == False and quotation_regex.search(word):
                    quoting = True
                elif quoting == True and quotation_regex.search(word):
                    quoting = False
                elif quoting and not word.endswith("_QUOTE"):
                    words[i] = word + "_QUOTE"
        # Nonsentiment punctuation:
        if self.filter_nonsentiment_punctuation:
            words = filter((lambda x : emoticon_re.search(x) or not nonsentiment_punctuation_re.search(x)), words)
        return words

    def date_normalizer(self, s):
        if long_date_re.search(s):
            try:
               dt = dateutil.parser.parse(s)
               return dt.strftime('%b_%d_%Y')
            except:
               return s            
        elif short_date_re.search(s):
            try:
               dt = dateutil.parser.parse(s)
               return dt.strftime('%b_%d')
            except:
               return s            
        else:
            return s
        
######################################################################

if __name__ == '__main__':
    # samples = (
    #     u"@Becky17 - i'm having a little trouble &quot;getting&quot;<br /> the whole twibes thing (but sometimes u gotta just get in there and try it).  :-)",
    #     u"Oh .. and follow @Spyker3292, @Domness, @Karlkempobrien, @Duidl_Media and @Chasetastic. Cheers for the Congrats! :D | #FollowSaturday",
    #     u"blade--trinity;;; sweeeeeeet. :)",
    #     u"@renay Thanks Renay! $9,000 yay =)",
    #     u"@denvy can try :) drop a tweet with \"##awaresg_tshirts\" so i can <strong>track</strong> orders #awaresg",
    #     u"U need to chk out &amp; follow here, a more beautiful animal not anywhere else! @EmmaRileySutton :) #followfriday",
    #     u"@LadyB84 Manchester United??? Really??? Breaks my heart to see a cute, smart, funny chick go bad:-(http://www.twitpic.com/4x1fn",
    #     u"Can&#39;t wait till tomorrow =D",
    #     u"Big Shot&#39;s Funeral » Google » Peoria making its case for Google ... http://cli.gs/Wa8za.",
    #     u"Contact email@address.org today",
    #     u"Variations on phone numbers: +1 (800) 123-4567, (800) 123-4567.",
    #     u"Some dates in a row to check the behavior: 2000-07-12, Oct 14, January 9, Dec 10, Nov 11, 2001, 12 Dec 2002, 5/8/1977, 1977/5/8.",
    #     u"RT @StanfordPraglab: Indian Independence Day is tomorrow --- 15 Aug 1947! http://en.wikipedia.org/wiki/Independence_Day_(India) #IndependenceDay :-)"
    #     )

    # tok = TwitterTokenizer(preserve_case=False, filter_html_tags=False,
    #                       filter_twitter_usernames=True, filter_twitter_hashtags=True,
    #                       filter_urls=False,
    #                       normalize_dates=True,
    #                       normalize_elogations=True)

    # for s in samples:
    #     print "======================================================================"
    #     print s
    #     tokenized = tok.tokenize(s)
    #     print "\n".join(tokenized)
    import sys
    text = sys.argv[1]
    print '======================================================================'
    print 'Sentiment'
    print "\n".join(TwitterTokenizer(preserve_case=False, preserve_all_caps=True, filter_html_tags=False,
                                     filter_twitter_usernames=False, filter_twitter_hashtags=False,
                                     filter_urls=False,
                                     normalize_dates=False,
                                     filter_dates=True,
                                     filter_nonsentiment_punctuation=True,
                                     normalize_elongations=True,
                                     mark_negation_scope=True).tokenize(text))
    print '======================================================================'
    print 'Whitespace'
    print "\n".join(WhitespaceTokenizer().tokenize(text))
    print '======================================================================'
    print 'No punctuation'
    print "\n".join(NopunctTokenizer().tokenize(text))
    print '======================================================================'
    print 'Treebank'
    print "\n".join(TreebankTokenizer().tokenize(text))
    
    
