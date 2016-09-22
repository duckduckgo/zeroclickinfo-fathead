"""
 Generate a quick HTML dump of a Fathead output.txt
"""
import csv
from parse import FatWriter

HTML = """
 <meta http-equiv='Content-Type' content='text/html; charset=utf-8'>
 <style>
   body {{
     font-family: 'Helvetica Neue', 'Segoe UI', sans-serif;
     font-size: 14px;
     border-top: 2px solid rgb(222, 88, 51);
     margin: 0;
   }}

.c-info,
.c-base,
.c-icon,
.c-list,
.c-product,
.c-detail,
.zci__main.has-aux,
.zci__main--answer,
.results,
.results--didyas,
.results--ads,
.zcm-wrap--header {{
    max-width: 590px
}}

.info:after,
.metabar__in:after,
.result__body:after,
.zcm--sub:after,
.zcm__menu:after,
.zci:after,
.zci__body:after,
.tile-wrap:after,
.modules__about__content:after {{
    content: "";
    display: block;
    clear: both
}}

.metabar__in,
.zcm-wrap,
.zci__main,
.zci__detail,
.results-wrapper,
.results--sidebar--mid {{
    padding-left: 94px
}}

.results--gutter {{
    display: block;
    width: 94px;
    position: absolute;
    top: 0
}}

.results--gutter {{
    left: 0
}}

@media only screen and (max-width: 1079px) {{
    .metabar__in,
    .zcm-wrap,
    .zci__main,
    .zci__detail,
    .results-wrapper,
    .results--sidebar--mid {{
        padding-left: 58px
    }}
    .results--gutter {{
        width: 58px;
        left: 0
    }}
}}

@media only screen and (max-width: 590px) {{
    .metabar__in,
    .zcm-wrap,
    .zci__main,
    .zci__detail,
    .results-wrapper,
    .results--sidebar--mid {{
        padding-left: 58px
    }}
    .results--gutter {{
        width: 58px;
        left: 0
    }}
    .metabar__in,
    .zcm-wrap,
    .zci__main,
    .zci__detail,
    .results-wrapper,
    .results--sidebar--mid {{
        padding-left: 0 !important
    }}
}}

@media only screen and (max-width: 425px) {{
    .metabar__in,
    .zcm-wrap,
    .zci__main,
    .zci__detail,
    .results-wrapper,
    .results--sidebar--mid {{
        padding-left: 58px
    }}
    .results--gutter {{
        width: 58px;
        left: 0
    }}
}}

.result__extras__url,
.zci__body__left,
.zci__body__right,
.result__menu {{
    float: left;
    position: relative;
    -webkit-box-sizing: border-box;
    -moz-box-sizing: border-box;
    -ms-box-sizing: border-box;
    -o-box-sizing: border-box;
    box-sizing: border-box
}}

.result__extras__url {{
    width: 70%
}}

.zci__body__left {{
    width: 60%
}}

.zci__body__right {{
    width: 40%
}}

.result__menu {{
    width: 30%
}}

@media only screen and (max-width: 590px) {{
    .result__extras__url {{
        width: 80%
    }}
    .result__menu {{
        width: 20%
    }}
}}

.tile--info__link:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale
}}

@media only screen and (max-width: 864px) {{
    .search__filters,
    .results--sidebar,
    .results--sidebar--alt {{
        display: none
    }}
}}

@media only screen and (max-width: 425px) {{
    .zcm__menu--tiles {{
        display: none
    }}
}}

.result__check__tt {{
    -moz-transition: opacity 0.3s ease-in-out 0s;
    -o-transition: opacity 0.3s ease-in-out 0s;
    -webkit-transition: opacity 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: opacity 0.3s ease-in-out 0s;
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    visibility: hidden;
    opacity: 0;
    display: inline-block;
    vertical-align: middle;
    position: absolute;
    margin: auto;
    background-color: #a3a3a3;
    background-color: rgba(138, 138, 138, 0.9);
    text-indent: 0px;
    padding: 0 1em;
    white-space: nowrap;
    line-height: 1.6;
    height: 1.6em;
    font-weight: 400;
    font-style: normal;
    color: white;
    z-index: 200
}}

.result__check__tt:before {{
    content: "";
    display: block;
    position: absolute;
    margin-left: -0.5em;
    bottom: -0.5em;
    left: 1.5em;
    border: 0.5em solid transparent;
    border-bottom-width: 0;
    border-top-color: #a3a3a3;
    border-top-color: rgba(138, 138, 138, 0.9)
}}

.has-tiles .anchor--inline {{
    left: -94px
}}

@media only screen and (max-width: 864px) {{
    .has-tiles .anchor--inline {{
        left: -58px
    }}
}}

@media only screen and (max-width: 590px) {{
    .has-tiles .anchor--inline {{
        left: -58px
    }}
}}

@media only screen and (max-width: 425px) {{
    .has-tiles .anchor--inline {{
        left: -58px
    }}
}}

.date-badge {{
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    color: #999;
    width: 34px;
    height: 34px;
    border: 1px solid #d0d0d0;
    display: block;
    text-align: center;
    white-space: nowrap
}}

.date-badge__text {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    line-height: 34px;
    height: 34px;
    max-width: 90%;
    display: block;
    margin: auto
}}

.date-badge__month,
.date-badge__day {{
    line-height: 17px;
    height: 17px;
    font-size: 11px;
    display: block
}}

.date-badge__day {{
    border-top: 1px solid #d0d0d0;
    margin-top: -1px;
    font-size: 13px
}}

.chomp {{
    overflow: hidden;
    # max-height: 7.75em
}}

.has-chomp-expanded .chomp,
.chomp.is-expanded {{
    max-height: none !important
}}

.chomp--scroll {{
    max-height: 216px;
    max-height: 15rem;
    overflow: auto
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .chomp--scroll {{
        max-height: 151.2px;
        max-height: 10.5rem
    }}
}}

.chomp__inner {{
    width: 100%
}}

.chomp--link {{
    display: none;
    cursor: pointer
}}

.chomp--link.can-expand {{
    display: inline-block
}}

.chomp--link__ls {{
    display: none
}}

.has-chomp-expanded .chomp--link .chomp--link__ls,
.chomp--link.is-expanded .chomp--link__ls {{
    display: inline
}}

.chomp--link__mr {{
    display: inline
}}

.has-chomp-expanded .chomp--link .chomp--link__mr,
.chomp--link.is-expanded .chomp--link__mr {{
    display: none
}}

.chomp--link__icn {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    text-align: center;
    display: inline-block;
    vertical-align: middle;
    margin-right: 0.5em;
    position: relative;
    line-height: 13px;
    width: 16px;
    height: 16px
}}

.chomp--link__icn:before {{
    content: "\2295";
    display: inline;
    font-size: 16px;
    color: #aaabab
}}

.has-chomp-expanded .chomp--link .chomp--link__icn:before,
.chomp--link.is-expanded .chomp--link__icn:before {{
    content: "\229d"
}}

.chomp--link__mr,
.chomp--link__ls {{
    color: #888
}}

.chomp--link__mr:hover,
.chomp--link:hover .chomp--link__mr,
.chomp--link__ls:hover,
.chomp--link:hover .chomp--link__ls {{
    color: #4495d4
}}

.badge,
.results--powered__badge {{
    text-indent: -999999px;
    display: inline-block;
    vertical-align: middle;
    position: relative;
    background-position: 50% 50%;
    background-repeat: no-repeat
}}

.badge--txt,
.badge--official,
.badge--ad,
.result__type,
.result__pagenum {{
    -moz-border-radius: 3px;
    -webkit-border-radius: 3px;
    border-radius: 3px;
    font-size: 12px;
    font-size: 0.83333rem;
    display: inline-block;
    vertical-align: middle;
    position: relative;
    text-indent: 0;
    margin-top: -1px;
    padding: 0 0.6em;
    white-space: nowrap;
    line-height: 1.6;
    font-weight: 400;
    font-style: normal;
    color: white;
    -webkit-user-select: none;
    -khtml-user-select: none;
    -moz-user-select: -moz-none;
    -ms-user-select: none;
    user-select: none
}}

.badge--ad.badge--ad--lt,
.badge--ad.badge--ad--rt,
.badge--ad.badge--ad--lu {{
    font-size: 10px;
    height: 15px;
    box-sizing: border-box;
    padding: 2px 3px 0;
    text-align: center
}}

.badge--official {{
    background-color: #de5833
}}

.badge--ad {{
    background-color: #f0a630
}}

.badge--ad,
.badge--ad:hover {{
    color: white
}}

.badge--ad.badge--ad--b {{
    background-color: #60a5da
}}

.badge--ad.badge--ad--g {{
    background-color: #888
}}

.badge--ad.badge--ad--lt {{
    margin-right: 0.6em;
    margin-top: -1px
}}

.badge--ad.badge--ad--rt {{
    float: none;
    top: 0px;
    line-height: 1;
    padding: 3px;
    height: 16px;
    margin-left: 0.1em
}}

.badge--ad.badge--ad--lu {{
    margin-right: 0.6em
}}

.badge--prime {{
    background-image: url("/assets/icons/thirdparty/prime.v101.png");
    background-size: 44px 12px;
    width: 44px;
    height: 12px
}}

.svg .badge--prime {{
    background-image: url("/assets/icons/thirdparty/prime.v101.svg")
}}

.badge--yandex {{
    background-image: url("/assets/attribution/yandex.v102.png");
    background-size: 44px 19px;
    width: 44px;
    height: 19px
}}

.svg .badge--yandex {{
    background-image: url("/assets/attribution/yandex.v102.svg")
}}

.badge--yahoo {{
    background-image: url("/assets/attribution/yahoo.v103.png");
    background-size: 55px 13px;
    width: 55px;
    height: 13px
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    .badge--yahoo {{
        background-image: url("/assets/attribution/yahoo.v103.svg")
    }}
}}

.badge--blekko {{
    background-image: url("/assets/attribution/blekko.v102.png");
    background-size: 48px 14px;
    width: 48px;
    height: 14px
}}

.svg .badge--blekko {{
    background-image: url("/assets/attribution/blekko.v102.svg")
}}

.badge--bing {{
    background-image: url("/assets/attribution/bing.v102.png");
    background-size: 54px 22px;
    width: 54px;
    height: 22px
}}

.svg .badge--bing {{
    background-image: url("/assets/attribution/bing.v102.svg")
}}

.play-btn {{
    display: inline-block;
    position: relative
}}

.play-btn__icn {{
    -moz-transform: scale(1);
    -ms-transform: scale(1);
    -webkit-transform: scale(1);
    transform: scale(1);
    -moz-transition: all 0.3s ease-in-out 0s;
    -o-transition: all 0.3s ease-in-out 0s;
    -webkit-transition: all 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: all 0.3s ease-in-out 0s;
    -moz-border-radius: 1.8em;
    -webkit-border-radius: 1.8em;
    border-radius: 1.8em;
    display: inline-block;
    margin: 0 0 0 0.5em;
    font-size: 0.5em;
    text-align: center;
    vertical-align: middle;
    background: #888;
    color: #f2f2f2;
    width: 1em;
    height: 1em;
    line-height: 1;
    padding: .4em;
    cursor: pointer
}}

.play-btn__icn:hover {{
    background: #aaa;
    color: #fff
}}

.play-btn__icn.is-loading {{
    -moz-transform: scale(0.5);
    -ms-transform: scale(0.5);
    -webkit-transform: scale(0.5);
    transform: scale(0.5);
    background: #de5833
}}

.play-btn__icn.is-playing,
.play-btn__icn.is-playing:hover {{
    -moz-transform: scale(0.8);
    -ms-transform: scale(0.8);
    -webkit-transform: scale(0.8);
    transform: scale(0.8);
    background: #5b9e4d
}}

.play-btn__err {{
    color: #888;
    font-style: italic;
    font-size: .5em
}}

.review-count {{
    text-decoration: inherit
}}

.review-count__icon {{
    display: inline-block;
    vertical-align: middle;
    text-decoration: none;
    margin-right: 3px
}}

.ddh-fathead li:before {{
    content: ".";
    color: #888;
    padding-right: 0.3em
}}

.ddh-fathead p strong {{
    color: #666;
    font-weight: normal
}}

.ddh-fathead p code {{
    color: #666
}}

.msg--serp,
.msg--result {{
    -webkit-tap-highlight-color: transparent;
    padding: 0.5em 10px;
    margin-bottom: 0;
    margin-left: 0
}}

@media only screen and (max-width: 590px) {{
    .msg--serp,
    .msg--result {{
        margin-top: 0
    }}
}}

.msg--site {{
    font-size: 1.1em
}}

.msg--result {{
    cursor: pointer
}}

.msg--result.highlight {{
    background-color: #f7f7f7
}}

.msg--result .msg__close {{
    right: 10px;
    color: #888;
    top: 7px
}}

.msg--result .msg__close:hover {{
    color: #333
}}

.msg--result a:visited {{
    color: #4495d4
}}

.msg--spelling {{
    font-size: 1.1em;
    cursor: default !important
}}

.msg--spelling a {{
    color: #4495d4
}}

.msg--spelling a:hover {{
    color: #4495d4;
    text-decoration: underline
}}

.msg--spelling a:active {{
    color: #60a5da
}}

.msg--transition-out {{
    opacity: 0;
    height: 0 !important
}}

.msg__close {{
    position: absolute;
    right: 1em;
    top: 1em;
    color: inherit
}}

.msg__wrap {{
    display: inline-block;
    position: relative;
    max-width: 100%;
    vertical-align: middle
}}

.lt-ie9 .msg__wrap {{
    padding-top: 0.5em;
    display: block
}}

.msg__btn-wrap {{
    display: inline-block;
    padding-top: .25em
}}

.msg__btn {{
    text-transform: capitalize;
    margin: 0 0.8em 0 0;
    font-size: 0.85em;
    line-height: 1.75;
    top: 2px;
    vertical-align: bottom
}}

.msg__label {{
    margin: 0 0.5em 0 0
}}

.msg__text {{
    margin-right: 12px
}}

.msg__clear {{
    font-weight: 600
}}

.msg__date {{
    text-transform: lowercase
}}

.msg__clear-filter,
.msg__clear-filter:hover {{
    text-decoration: none;
    color: #666
}}

.msg__clear-filter-x {{
    margin-left: 2px;
    font-size: 8px;
    font-family: 'ddg-serp-icons' !important
}}

.msg__sites--mobile {{
    display: block
}}

.msg__site {{
    color: #666;
    display: inline-block;
    padding: 0 12px 0 0
}}

.msg__all,
.msg__all:visited {{
    display: inline-block;
    color: #4495d4
}}

.msg__line {{
    display: block
}}

.msg__line--small {{
    font-size: 0.9em
}}

.detail--l .detail__media:after,
.detail--products .detail__media:after,
.detail--qa .detail__media:after,
.detail--about .detail__media:after,
.tile--img__media:after,
.tile--vid__overlay,
.tile__media--pr:after {{
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0;
    display: block
}}

.overlay,
.overlay__wrap {{
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0
}}

.overlay {{
    z-index: 1;
    display: block;
    overflow: hidden;
    background: #333;
    background: rgba(45, 45, 45, 0.9);
    text-align: center;
    color: #fafafa
}}

.is-mobile .overlay {{
    position: fixed
}}

.overlay__wrap {{
    display: table;
    height: 100%
}}

.overlay__content {{
    display: table-cell;
    vertical-align: middle;
    text-align: center;
    line-height: 1.2
}}

.overlay__title {{
    margin: 0.5em 0 0.25em;
    font-weight: 300
}}

.overlay__text {{
    font-weight: 300;
    color: #bbb
}}

.overlay__link {{
    color: #49a9f2
}}

.overlay__btn-list {{
    padding: 1em 0.5em
}}

.is-mobile .overlay__btn-list {{
    padding-left: 0;
    padding-right: 0
}}

.overlay__btn-list__li {{
    display: inline-block;
    width: 11em;
    margin: 0 .25em .5em
}}

.is-mobile .overlay__btn-list__li {{
    width: 46%;
    margin-left: 1%;
    margin-right: 1%
}}

.overlay__btn {{
    display: block;
    padding: .65em 0;
    text-align: center;
    line-height: 1.2;
    border-color: #fafafa;
    color: #fafafa
}}

.overlay__btn:hover {{
    background-color: #fafafa;
    color: #333
}}

.expand {{
    cursor: pointer
}}

.expand__icon {{
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
    margin-right: 0.5em;
    background-color: #dbdbdb;
    display: inline-block;
    position: relative;
    width: 1.25em;
    height: 1.25em;
    text-align: center
}}

.expand__icon:before {{
    content: "";
    width: 0;
    height: 0;
    top: -0.10em;
    left: 0.05em;
    position: relative;
    display: inline-block;
    border: 0.25em solid transparent;
    border-top-color: #464646;
    border-top-width: 0.4em;
    border-bottom-width: 0
}}

.is-open .expand__icon:before,
.expand__icon.is-open:before {{
    border-bottom-color: #464646;
    border-bottom-width: 0.4em;
    border-top-width: 0;
    top: -0.15em
}}

.expand:hover .expand__icon {{
    -moz-box-shadow: 0 0 1px #dbdbdb;
    -webkit-box-shadow: 0 0 1px #dbdbdb;
    box-shadow: 0 0 1px #dbdbdb;
    background-color: #e8e8e8
}}

.expand:active .expand__icon {{
    background-color: #464646
}}

.expand:active .expand__icon:before {{
    border-top-color: #dbdbdb
}}

.is-open .expand:active .expand__icon:before,
.expand:active .expand__icon.is-open:before {{
    border-bottom-color: #dbdbdb
}}

.expand__less {{
    display: none
}}

.is-open .expand__more {{
    display: none
}}

.is-open .expand__less {{
    display: inline
}}

.stars {{
    vertical-align: baseline;
    display: inline-block;
    line-height: inherit;
    position: relative;
    margin-right: 4px
}}

.star {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    vertical-align: middle;
    display: inline-block;
    position: relative;
    margin-right: -2px;
    font-size: 1em;
    color: #d0d0d0;
    line-height: 1;
    height: 1em;
    width: 1em
}}

.star:before,
.star:after {{
    display: block;
    position: absolute;
    content: ".";
    line-height: 1;
    height: 1em;
    width: 1em;
    margin: auto;
    bottom: 0;
    left: 0;
    top: 0
}}

.star:after {{
    color: #de5833;
    visibility: hidden;
    overflow: hidden
}}

.stars--1 .stars__1:after,
.stars--1-5 .stars__1:after,
.stars--2 .stars__1:after,
.stars--2-5 .stars__1:after,
.stars--3 .stars__1:after,
.stars--3-5 .stars__1:after,
.stars--4 .stars__1:after,
.stars--4-5 .stars__1:after,
.stars--5 .stars__1:after {{
    visibility: visible
}}

.stars--2 .stars__2:after,
.stars--2-5 .stars__2:after,
.stars--3 .stars__2:after,
.stars--3-5 .stars__2:after,
.stars--4 .stars__2:after,
.stars--4-5 .stars__2:after,
.stars--5 .stars__2:after {{
    visibility: visible
}}

.stars--3 .stars__3:after,
.stars--3-5 .stars__3:after,
.stars--4 .stars__3:after,
.stars--4-5 .stars__3:after,
.stars--5 .stars__3:after {{
    visibility: visible
}}

.stars--4 .stars__4:after,
.stars--4-5 .stars__4:after,
.stars--5 .stars__4:after {{
    visibility: visible
}}

.stars--5 .stars__5:after {{
    visibility: visible
}}

.stars--0-5 .stars__1:after {{
    visibility: visible;
    width: 0.5em
}}

.stars--1-5 .stars__2:after {{
    visibility: visible;
    width: 0.5em
}}

.stars--2-5 .stars__3:after {{
    visibility: visible;
    width: 0.5em
}}

.stars--3-5 .stars__4:after {{
    visibility: visible;
    width: 0.5em
}}

.stars--4-5 .stars__5:after {{
    visibility: visible;
    width: 0.5em
}}

.count {{
    vertical-align: baseline;
    display: inline-block;
    line-height: inherit;
    position: relative;
    margin-right: 4px
}}

.count__i {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    display: inline-block;
    vertical-align: middle;
    text-align: center;
    position: relative;
    font-size: 1em;
    color: #d0d0d0;
    line-height: 1;
    height: 1em;
    width: 1em
}}

.count__i:before,
.count__i:after {{
    display: block;
    font-size: 0.7em;
    position: absolute;
    content: ".";
    line-height: 1.15;
    height: 1.25em;
    width: 1.25em;
    margin: auto;
    bottom: 0;
    left: 0;
    top: 0
}}

.count__i:after {{
    color: #333;
    visibility: hidden;
    overflow: hidden
}}

.count--1 .count__1:after,
.count--2 .count__1:after,
.count--3 .count__1:after,
.count--4 .count__1:after,
.count--5 .count__1:after {{
    visibility: visible
}}

.count--2 .count__2:after,
.count--3 .count__2:after,
.count--4 .count__2:after,
.count--5 .count__2:after {{
    visibility: visible
}}

.count--3 .count__3:after,
.count--4 .count__3:after,
.count--5 .count__3:after {{
    visibility: visible
}}

.count--4 .count__4:after,
.count--5 .count__4:after {{
    visibility: visible
}}

.count--5 .count__5:after {{
    visibility: visible
}}

.count--green .count__i:after {{
    color: #5b9e4d
}}

.count--red .count__i:after {{
    color: #de5833
}}

.count--gold .count__i:after {{
    color: #f1a031
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .opt {{
        display: none !important
    }}
    .opt--left {{
        float: left !important
    }}
    .opt--right {{
        float: right !important
    }}
    .opt--t-l {{
        font-size: 1.33em !important
    }}
    .opt--t-m {{
        font-size: 1.1667em !important
    }}
    .opt--t-s {{
        font-size: 0.9176em !important
    }}
    .opt--t-xs {{
        font-size: 0.8333em !important
    }}
    .opt--t-xxs {{
        font-size: 0.75em !important
    }}
}}

.record {{
    display: block
}}

.record__body {{
    table-layout: fixed;
    width: 100%
}}

.record__cell {{
    text-overflow: ellipsis;
    word-wrap: break-word;
    white-space: nowrap;
    overflow: hidden;
    border-top: 1px solid #d0d0d0;
    border-bottom: 1px solid #d0d0d0;
    padding: 0.35em 0.35em 0.35em 0
}}

.is-expanded .record__cell {{
    white-space: normal
}}

.record--highlight .record__cell {{
    padding-left: 0.35em
}}

.record--keyspacing .record__cell--key {{
    padding-right: 1.5em;
    width: 7.7em
}}

.record__cell--key {{
    padding-right: 0.5em;
    width: 8.7em;
    text-align: left;
    vertical-align: top;
    text-transform: capitalize
}}

.record__row--highlight:nth-child(odd) {{
    background-color: rgba(150, 150, 150, 0.15)
}}

.info {{
    display: block;
    padding-top: 0.3em;
    padding-bottom: 0.3em;
    position: relative
}}

.info img {{
    display: none
}}

.info--head {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    font-size: 1em;
    font-weight: 600;
    padding: 0.3em 0 0.4em
}}

.info__label,
.info__value {{
    display: block;
    margin-top: 1px;
    margin-bottom: -1px
}}

.info__label {{
    color: gray;
    padding-right: 0.5em;
    text-align: left;
    float: left
}}

.is-mobile .info__label {{
    float: none;
    margin-top: -2px
}}

.info__value {{
    text-align: right;
    overflow: hidden
}}

.is-mobile .info__value {{
    text-align: left
}}

.info__value__nested {{
    display: block
}}

.info__value__nested__label {{
    color: gray
}}

.flag-sm {{
    display: inline-block;
    background: url("/assets/flags/flags-20px.png?v=3") no-repeat;
    width: 20px;
    height: 20px;
    background-size: 20px 1160px
}}

.flag-sm-ar {{
    background-position: 0px 0px
}}

.flag-sm-at {{
    background-position: 0px -20px
}}

.flag-sm-au {{
    background-position: 0px -40px
}}

.flag-sm-be {{
    background-position: 0px -60px
}}

.flag-sm-bg {{
    background-position: 0px -80px
}}

.flag-sm-br {{
    background-position: 0px -100px
}}

.flag-sm-ca {{
    background-position: 0px -120px
}}

.flag-sm-ch {{
    background-position: 0px -140px
}}

.flag-sm-cl {{
    background-position: 0px -160px
}}

.flag-sm-cn {{
    background-position: 0px -180px
}}

.flag-sm-co {{
    background-position: 0px -200px
}}

.flag-sm-ct {{
    background-position: 0px -220px
}}

.flag-sm-cz {{
    background-position: 0px -240px
}}

.flag-sm-de {{
    background-position: 0px -260px
}}

.flag-sm-dk {{
    background-position: 0px -280px
}}

.flag-sm-ee {{
    background-position: 0px -300px
}}

.flag-sm-es {{
    background-position: 0px -320px
}}

.flag-sm-fi {{
    background-position: 0px -340px
}}

.flag-sm-fr {{
    background-position: 0px -360px
}}

.flag-sm-gr {{
    background-position: 0px -380px
}}

.flag-sm-hk {{
    background-position: 0px -400px
}}

.flag-sm-hr {{
    background-position: 0px -420px
}}

.flag-sm-hu {{
    background-position: 0px -440px
}}

.flag-sm-id {{
    background-position: 0px -460px
}}

.flag-sm-ie {{
    background-position: 0px -480px
}}

.flag-sm-il {{
    background-position: 0px -500px
}}

.flag-sm-in {{
    background-position: 0px -520px
}}

.flag-sm-it {{
    background-position: 0px -540px
}}

.flag-sm-jp {{
    background-position: 0px -560px
}}

.flag-sm-kr {{
    background-position: 0px -580px
}}

.flag-sm-lt {{
    background-position: 0px -600px
}}

.flag-sm-lv {{
    background-position: 0px -620px
}}

.flag-sm-mx {{
    background-position: 0px -640px
}}

.flag-sm-my {{
    background-position: 0px -660px
}}

.flag-sm-nl {{
    background-position: 0px -680px
}}

.flag-sm-no {{
    background-position: 0px -700px
}}

.flag-sm-nz {{
    background-position: 0px -720px
}}

.flag-sm-pe {{
    background-position: 0px -740px
}}

.flag-sm-ph {{
    background-position: 0px -760px
}}

.flag-sm-pl {{
    background-position: 0px -780px
}}

.flag-sm-pt {{
    background-position: 0px -800px
}}

.flag-sm-ro {{
    background-position: 0px -820px
}}

.flag-sm-ru {{
    background-position: 0px -840px
}}

.flag-sm-se {{
    background-position: 0px -860px
}}

.flag-sm-sg {{
    background-position: 0px -880px
}}

.flag-sm-sk {{
    background-position: 0px -900px
}}

.flag-sm-sl {{
    background-position: 0px -920px
}}

.flag-sm-th {{
    background-position: 0px -940px
}}

.flag-sm-tr {{
    background-position: 0px -960px
}}

.flag-sm-tw {{
    background-position: 0px -980px
}}

.flag-sm-ua {{
    background-position: 0px -1000px
}}

.flag-sm-uk {{
    background-position: 0px -1020px
}}

.flag-sm-us {{
    background-position: 0px -1040px
}}

.flag-sm-vn {{
    background-position: 0px -1060px
}}

.flag-sm-wt {{
    background-position: 0px -1080px
}}

.flag-sm-xa {{
    background-position: 0px -1100px
}}

.flag-sm-xl {{
    background-position: 0px -1120px
}}

.flag-sm-za {{
    background-position: 0px -1140px
}}

.flag-lg {{
    display: inline-block;
    background: url("/assets/flags/flags-32px.png?v=3") no-repeat;
    width: 32px;
    height: 32px;
    background-size: 32px 1824px
}}

.flag-lg-ar {{
    background-position: 0px 0px
}}

.flag-lg-at {{
    background-position: 0px -32px
}}

.flag-lg-au {{
    background-position: 0px -64px
}}

.flag-lg-be {{
    background-position: 0px -96px
}}

.flag-lg-bg {{
    background-position: 0px -128px
}}

.flag-lg-br {{
    background-position: 0px -160px
}}

.flag-lg-ca {{
    background-position: 0px -192px
}}

.flag-lg-ch {{
    background-position: 0px -224px
}}

.flag-lg-cl {{
    background-position: 0px -256px
}}

.flag-lg-cn {{
    background-position: 0px -288px
}}

.flag-lg-co {{
    background-position: 0px -320px
}}

.flag-lg-ct {{
    background-position: 0px -352px
}}

.flag-lg-cz {{
    background-position: 0px -384px
}}

.flag-lg-de {{
    background-position: 0px -416px
}}

.flag-lg-dk {{
    background-position: 0px -448px
}}

.flag-lg-ee {{
    background-position: 0px -480px
}}

.flag-lg-es {{
    background-position: 0px -512px
}}

.flag-lg-fi {{
    background-position: 0px -544px
}}

.flag-lg-fr {{
    background-position: 0px -576px
}}

.flag-lg-gr {{
    background-position: 0px -608px
}}

.flag-lg-hk {{
    background-position: 0px -640px
}}

.flag-lg-hr {{
    background-position: 0px -672px
}}

.flag-lg-hu {{
    background-position: 0px -704px
}}

.flag-lg-id {{
    background-position: 0px -736px
}}

.flag-lg-ie {{
    background-position: 0px -768px
}}

.flag-lg-il {{
    background-position: 0px -800px
}}

.flag-lg-in {{
    background-position: 0px -832px
}}

.flag-lg-it {{
    background-position: 0px -864px
}}

.flag-lg-jp {{
    background-position: 0px -896px
}}

.flag-lg-kr {{
    background-position: 0px -928px
}}

.flag-lg-lt {{
    background-position: 0px -960px
}}

.flag-lg-lv {{
    background-position: 0px -992px
}}

.flag-lg-my {{
    background-position: 0px -1024px
}}

.flag-lg-nl {{
    background-position: 0px -1056px
}}

.flag-lg-no {{
    background-position: 0px -1088px
}}

.flag-lg-nz {{
    background-position: 0px -1120px
}}

.flag-lg-pe {{
    background-position: 0px -1152px
}}

.flag-lg-ph {{
    background-position: 0px -1184px
}}

.flag-lg-pl {{
    background-position: 0px -1216px
}}

.flag-lg-pt {{
    background-position: 0px -1248px
}}

.flag-lg-ro {{
    background-position: 0px -1280px
}}

.flag-lg-ru {{
    background-position: 0px -1312px
}}

.flag-lg-se {{
    background-position: 0px -1344px
}}

.flag-lg-sg {{
    background-position: 0px -1376px
}}

.flag-lg-sk {{
    background-position: 0px -1408px
}}

.flag-lg-sl {{
    background-position: 0px -1440px
}}

.flag-lg-th {{
    background-position: 0px -1472px
}}

.flag-lg-tr {{
    background-position: 0px -1504px
}}

.flag-lg-tw {{
    background-position: 0px -1536px
}}

.flag-lg-ua {{
    background-position: 0px -1568px
}}

.flag-lg-uk {{
    background-position: 0px -1600px
}}

.flag-lg-us {{
    background-position: 0px -1632px
}}

.flag-lg-vn {{
    background-position: 0px -1664px
}}

.flag-lg-wt {{
    background-position: 0px -1696px
}}

.flag-lg-xa {{
    background-position: 0px -1728px
}}

.flag-lg-xl {{
    background-position: 0px -1760px
}}

.flag-lg-za {{
    background-position: 0px -1792px
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    .flag-sm {{
        background-image: url("/assets/flags/flags-40px.png?v=3")
    }}
    .flag-lg {{
        background-image: url("/assets/flags/flags-64px.png?v=3")
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 3),
only screen and (-moz-min-device-pixel-ratio: 3),
only screen and (min--moz-device-pixel-ratio: 3),
only screen and (-ms-min-device-pixel-ratio: 3),
only screen and (min-device-pixel-ratio: 3),
only screen and (min-resolution: 288dppx) {{
    .flag-sm {{
        background-image: url("/assets/flags/flags-60px.png?v=3")
    }}
    .flag-lg {{
        background-image: url("/assets/flags/flags-96px.png?v=3")
    }}
}}

.attribution-wrap--top,
.attribution-wrap--infobox {{
    margin-right: 10px;
    position: absolute;
    right: 7px;
    top: 1em;
    height: 24px
}}

@media only screen and (min-width: 980px) {{
    .attribution-wrap--infobox {{
        margin-right: 1.25em;
        right: 27%
    }}
}}

.attribution {{
    padding: 0.5em;
    line-height: 1.5;
    font-size: 0.9em;
    text-align: left
}}

.attribution--link {{
    cursor: pointer
}}

.attribution--link:hover {{
    text-decoration: none
}}

.attribution--link:hover b {{
    text-decoration: underline
}}

.attribution--link__icon {{
    position: relative;
    color: #aaa;
    font-size: 16px
}}

.attribution--link__icon:hover,
.attribution--link__icon:focus {{
    color: #999
}}

.attribution--link__icon:active {{
    color: #888
}}

.attribution__hr {{
    border: none;
    margin: 1.25em 0;
    border-bottom: 1px solid #ededed
}}

.attribution__btn {{
    line-height: 1.8
}}

.metabar {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    min-height: 33px;
    padding-top: 5px;
    padding-bottom: 1px;
    padding-left: 7px;
    padding-right: 7px;
    top: 0;
    right: 0;
    left: 0;
    z-index: 10
}}

.metabar--sticky {{
    position: -webkit-sticky;
    position: -moz-sticky;
    position: -ms-sticky;
    position: -o-sticky;
    position: sticky
}}

.metabar--fixed {{
    position: static
}}

.metabar__in {{
    -moz-transition: padding-left 0.3s ease-in-out 0s;
    -o-transition: padding-left 0.3s ease-in-out 0s;
    -webkit-transition: padding-left 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: padding-left 0.3s ease-in-out 0s;
    line-height: 33px;
    height: 33px;
    font-size: 0.9em;
    visibility: hidden;
    padding-bottom: 0;
    padding-top: 0;
    margin-top: 0;
    clear: both;
    z-index: 2
}}

.is-mobile-device .metabar__in {{
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s
}}

.is-active .metabar__in {{
    visibility: visible
}}

.metabar__in.is-expanded {{
    padding-left: 0
}}

@media only screen and (max-width: 590px) {{
    .metabar--fixed .metabar__in {{
        margin-left: 0.5em
    }}
    .is-stuck>.metabar__in {{
        margin-left: 0
    }}
}}

.metabar__item-type {{
    text-transform: capitalize
}}

.metabar__term {{
    font-weight: 600
}}

.metabar__primary-text,
.metabar__secondary-text {{
    padding-top: 0;
    overflow: hidden;
    font-size: 1.1em;
    line-height: 33px;
    max-height: 33px
}}

.metabar__primary-text.is-loading,
.metabar__secondary-text.is-loading {{
    display: none
}}

.metabar__primary-text {{
    display: block;
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden;
    padding-left: 10px;
    padding-right: 0.5em
}}

.has-dropdowns .metabar__primary-text {{
    display: inline-block;
    overflow: auto;
    padding-right: 0;
    color: #999
}}

.metabar__secondary-text,
.metabar__attribution {{
    line-height: 33px;
    height: 33px;
    font-size: 0.9176em;
    float: right
}}

.metabar__attribution .modal-trig {{
    height: 28px
}}

.metabar__attribution__sep {{
    margin-top: 9px;
    float: right
}}

.metabar__more-at {{
    line-height: 33px;
    float: right
}}

.metabar__mode-wrap {{
    padding-left: 1em;
    line-height: 33px;
    height: 33px;
    float: right
}}

.mapview__close,
.metabar__mode {{
    background-color: #dfdfdf;
    color: #666;
    vertical-align: top;
    top: 0
}}

.is-disabled.mapview__close,
.is-disabled.mapview__close:hover,
.is-disabled.mapview__close:active,
.metabar__mode.is-disabled,
.metabar__mode.is-disabled:hover,
.metabar__mode.is-disabled:active {{
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s;
    color: #dadada;
    background-color: transparent;
    cursor: default
}}

.metabar__dropdowns {{
    display: inline-block;
    margin-left: 1em;
    vertical-align: top
}}

.metabar__dropdowns.is-loading {{
    display: none
}}

.tile-nav,
.tile-nav--sm {{
    -moz-transform: translate3d(0, 0, 140px);
    -webkit-transform: translate3d(0, 0, 140px);
    transform: translate3d(0, 0, 140px);
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    -moz-transition: all 0.3s ease-in-out 0s;
    -o-transition: all 0.3s ease-in-out 0s;
    -webkit-transition: all 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: all 0.3s ease-in-out 0s;
    font-style: normal;
    -webkit-user-select: none;
    -khtml-user-select: none;
    -moz-user-select: -moz-none;
    -ms-user-select: none;
    user-select: none
}}

.can-scroll.tile-nav,
.can-scroll.tile-nav--sm {{
    cursor: pointer
}}

.tile-nav:before,
.tile-nav--sm:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    line-height: 1;
    position: relative
}}

.tile-nav {{
    -moz-perspective: 1000;
    -webkit-perspective: 1000;
    perspective: 1000;
    -moz-border-radius: 5em;
    -webkit-border-radius: 5em;
    border-radius: 5em;
    font-size: 1.2em;
    cursor: default;
    position: absolute;
    z-index: 10;
    top: 0;
    bottom: 0;
    margin: auto;
    width: 6em;
    height: 4em;
    line-height: 4;
    padding: 0 0.8em;
    color: #f2f2f2;
    color: rgba(242, 242, 242, 0.5);
    background-color: #c6c6c6;
    background-color: rgba(35, 35, 35, 0.1);
    display: none;
    -moz-transition-delay: 0.5s;
    -o-transition-delay: 0.5s;
    -webkit-transition-delay: 0.5s;
    transition-delay: 0.5s
}}

.tile-nav,
.tile-nav:after {{
    -moz-backface-visibility: hidden;
    -webkit-backface-visibility: hidden;
    backface-visibility: hidden;
    -moz-transform-style: preserve-3d;
    -webkit-transform-style: preserve-3d;
    transform-style: preserve-3d
}}

.tile-nav:after {{
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
    -moz-transition: all 0.3s ease-in-out 0s;
    -o-transition: all 0.3s ease-in-out 0s;
    -webkit-transition: all 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: all 0.3s ease-in-out 0s;
    -moz-transform: scale(0.01, 0.01);
    -ms-transform: scale(0.01, 0.01);
    -webkit-transform: scale(0.01, 0.01);
    transform: scale(0.01, 0.01);
    display: none;
    position: absolute;
    margin: auto;
    bottom: 0;
    top: 0;
    text-align: center;
    font-size: 0.695em;
    width: 2em;
    height: 2em;
    line-height: 2
}}

.tile-nav:before {{
    top: 0.25em
}}

.tile-nav.can-scroll,
.tile-nav.can-scroll:after {{
    color: #fff;
    background-color: #de5833
}}

.tile-nav.can-scroll:hover,
.tile-nav.can-scroll:hover:after {{
    background-color: #de5833
}}

.tile-nav.can-scroll:active {{
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s;
    background-color: black
}}

.tile-nav.can-scroll:after {{
    content: attr(data-items);
    display: block
}}

.tile-nav.can-scroll:hover:after {{
    -moz-transform: scale(1, 1);
    -ms-transform: scale(1, 1);
    -webkit-transform: scale(1, 1);
    transform: scale(1, 1)
}}

.no-touch .has-tiles .tile-nav {{
    display: block
}}

.has-tiles:hover .tile-nav,
.tile-nav:hover {{
    -moz-transition-delay: 0.001s;
    -o-transition-delay: 0.001s;
    -webkit-transition-delay: 0.001s;
    transition-delay: 0.001s
}}

.tile-nav--d {{
    position: absolute;
    top: 50%;
    margin: -2em 0 0 0;
    background-color: rgba(34, 34, 34, 0.75) !important
}}

.tile-nav--d:after {{
    display: none !important
}}

.tile-nav--d.can-scroll {{
    display: block
}}

.tile-nav--d.tile-nav--prev {{
    left: -3em
}}

.tile-nav--d.tile-nav--prev:before {{
    content: "\2039";
    padding-right: .25em
}}

.tile-nav--d.tile-nav--next {{
    right: -3em
}}

.tile-nav--d.tile-nav--next:before {{
    content: "\203a";
    padding-left: .25em
}}

.tile-nav--sm {{
    cursor: default;
    position: relative;
    margin-left: 0.5em;
    opacity: 0.25
}}

.tile-nav--sm:before {{
    font-size: 1.2em;
    top: 0.15em
}}

.tile-nav--sm.can-scroll {{
    cursor: pointer;
    opacity: 1
}}

.no-touch .tile-nav--sm.can-scroll:hover {{
    color: #fff;
    background-color: #6d6d6d
}}

.no-touch .tile-nav--sm.can-scroll:hover:active {{
    background-color: #555
}}

.tile-nav--prev {{
    text-align: right;
    left: -7em
}}

.tile-nav--prev:before {{
    content: "\3c"
}}

.has-tiles:hover .tile-nav--prev.can-scroll {{
    left: -3.75em
}}

.tile-nav--prev:after {{
    right: 6em
}}

.tile-nav--prev:hover:after {{
    right: -2.5em
}}

.tile-nav--sm--prev:before {{
    content: "\ab"
}}

.tile-nav--next {{
    text-align: left;
    right: -7em
}}

.tile-nav--next:before {{
    content: "\3e"
}}

.has-tiles:hover .tile-nav--next.can-scroll {{
    right: -3.75em
}}

.tile-nav--next:after {{
    left: 6em
}}

.tile-nav--next:hover:after {{
    left: -2.5em
}}

.tile-nav--sm--next:before {{
    content: "\bb"
}}

.tile--news .tile__media__plc,
.tile--audio .tile__media__no-artwork {{
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0;
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
    background-color: #e0e0e0;
    color: #fff;
    font-size: 3em;
    height: 1em;
    width: 1em;
    padding: 0.5em;
    line-height: 1
}}

.tile,
.tile--s,
.tile--info,
.tile--map {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    display: block;
    position: relative;
    background-color: #fff;
    border: 1px solid #e3e3e3;
    border-bottom-color: #cacaca;
    font-size: 0.87em;
    margin: 8px 5px;
    line-height: 1.37;
    padding: 0;
    opacity: 1
}}

.highlight.tile,
.highlight.tile--s,
.highlight.tile--info,
.highlight.tile--map {{
    -moz-border-radius: 0;
    -webkit-border-radius: 0;
    border-radius: 0;
    -moz-box-shadow: 0 0 0 1px #595959;
    -webkit-box-shadow: 0 0 0 1px #595959;
    box-shadow: 0 0 0 1px #595959;
    border-color: #595959
}}

.highlight.tile:active,
.highlight.tile--s:active,
.highlight.tile--info:active,
.highlight.tile--map:active {{
    -moz-box-shadow: 0 0 0 1px #de5833;
    -webkit-box-shadow: 0 0 0 1px #de5833;
    box-shadow: 0 0 0 1px #de5833;
    border-color: #de5833
}}

.is-selected.tile,
.is-selected.tile--s,
.is-selected.tile--info,
.is-selected.tile--map {{
    -moz-border-radius: 0;
    -webkit-border-radius: 0;
    border-radius: 0;
    -moz-box-shadow: 0 0 0 1px #494949;
    -webkit-box-shadow: 0 0 0 1px #494949;
    box-shadow: 0 0 0 1px #494949;
    border-color: #494949;
    z-index: 5
}}

.is-mobile .is-selected.tile,
.is-mobile .is-selected.tile--s,
.is-mobile .is-selected.tile--info,
.is-mobile .is-selected.tile--map {{
    -moz-box-shadow: none;
    -webkit-box-shadow: none;
    box-shadow: none
}}

.is-requerying .tile,
.is-requerying .tile--s,
.is-requerying .tile--info,
.is-requerying .tile--map {{
    -moz-transition: opacity 0.3s ease-in-out 0s;
    -o-transition: opacity 0.3s ease-in-out 0s;
    -webkit-transition: opacity 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: opacity 0.3s ease-in-out 0s;
    filter: progid: DXImageTransform.Microsoft.Alpha(Opacity=20);
    opacity: 0.2
}}

.tile .tile,
.tile--s .tile,
.tile--info .tile,
.tile--map .tile {{
    max-width: 100%;
    font-size: 1em;
    margin: 0
}}

.tile .zci__body,
.tile--s .zci__body,
.tile--info .zci__body,
.tile--map .zci__body {{
    padding-left: 0
}}

.tile--no-highlight.highlight,
.tile--no-highlight.highlight:active,
.tile--no-highlight.is-selected {{
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    -moz-box-shadow: none;
    -webkit-box-shadow: none;
    box-shadow: none;
    border-color: #e3e3e3;
    border-bottom-color: #cacaca
}}

.tile__media {{
    margin: auto;
    display: block;
    width: 100%;
    max-width: 100%;
    overflow: hidden;
    position: relative;
    height: 9em
}}

.tile__media__img {{
    display: block;
    margin: 0 auto;
    color: transparent
}}

.tile__media__img:-moz-loading {{
    visibility: hidden
}}

.tile__num {{
    position: absolute;
    top: 0;
    left: 0;
    font-weight: 600;
    color: #333;
    width: 2em;
    height: 2.5em;
    padding: 0.1em 1em 0 0;
    line-height: 2em;
    text-align: center;
    overflow: hidden;
    z-index: 1
}}

.tile__num:before {{
    -moz-transform: skew(135deg);
    -ms-transform: skew(135deg);
    -webkit-transform: skew(135deg);
    transform: skew(135deg);
    content: "";
    display: block;
    position: absolute;
    background: #fff;
    height: 100%;
    width: 100%;
    bottom: 0;
    left: -15px;
    top: 0;
    z-index: -1
}}

.tile__body,
.tile--s,
.tile--info {{
    -moz-transform-style: inherit;
    -webkit-transform-style: inherit;
    transform-style: inherit;
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    padding: 1em;
    word-wrap: break-word;
    word-break: initial;
    overflow: hidden
}}

.tile__body .tile__body,
.tile--s .tile__body,
.tile--info .tile__body,
.tile__body .tile--s,
.tile--s .tile--s,
.tile--info .tile--s,
.tile__body .tile--info,
.tile--s .tile--info,
.tile--info .tile--info {{
    padding: 0
}}

.tile__body.can-expand {{
    padding-bottom: 3em
}}

.is-mobile .tile__body {{
    font-size: 0.9em
}}

.tile__body.has-foot {{
    padding-bottom: 2.37em
}}

.tile__body.has-foot--2 {{
    padding-bottom: 3.74em
}}

.tile__body.has-foot--3 {{
    padding-bottom: 5.11em
}}

.tile__body.has-foot--4 {{
    padding-bottom: 6.48em
}}

.tile__body.has-segments {{
    padding-left: 0;
    padding-right: 0
}}

.tile__body .date-badge {{
    position: absolute;
    bottom: 1em;
    right: 1em
}}

.tile__foot {{
    max-height: 2.37em;
    position: absolute;
    height: auto;
    bottom: 0;
    right: 0;
    left: 0;
    padding-top: 3px;
    padding-bottom: 0.75em;
    padding-left: 1em;
    padding-right: 1em;
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis
}}

.tile__foot--2 {{
    max-height: 3.74em;
    white-space: normal
}}

.tile__foot--3 {{
    max-height: 5.11em;
    white-space: normal
}}

.tile__foot--4 {{
    max-height: 6.48em;
    white-space: normal
}}

.tile__title {{
    font-weight: 600;
    color: #333;
    padding: 0;
    overflow: hidden;
    line-height: 1.25;
    height: 2.5em;
    margin-bottom: 0.7em;
    margin-top: 0;
    text-overflow: ellipsis;
    text-overflow: -o-ellipsis-lastline;
    display: block;
    display: -webkit-box;
    -webkit-line-clamp: 2;
    -webkit-box-orient: vertical
}}

.tile__title a {{
    color: inherit
}}

.tile__title a:hover {{
    text-decoration: underline;
    color: inherit
}}

.tile__title a:visited {{
    color: #888
}}

.is-mobile .tile__title {{
    margin-bottom: 0.25em
}}

.tile__title.with-sub,
.is-mobile .tile__title.with-sub {{
    margin-bottom: 0
}}

.tile__title.has-sub--a {{
    padding-bottom: 18.72px;
    padding-bottom: 1.3rem
}}

.has-sub--a .tile__title__main {{
    max-height: 100%;
    overflow: hidden;
    display: block
}}

.tile__sub,
.tile__title__sub,
.tile__neighborhood,
.tile__sub--2 {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    text-transform: capitalize;
    font-weight: normal;
    display: block;
    color: #999
}}

.tile__sub {{
    margin-bottom: 1em;
    height: 1.3em
}}

.tile__sub--2 {{
    white-space: normal;
    margin-bottom: 0.4em;
    padding-bottom: 0;
    height: 2.6em
}}

.tile__title--1,
.tile__title--1lg {{
    display: block;
    white-space: nowrap;
    height: 1.25em
}}

.tile__title--1 {{
    font-size: 1.2em
}}

.tile__title--1lg {{
    font-size: 1.5em
}}

.tile__title--2 {{
    font-size: 1.2em
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .tile__title--2 {{
        display: block;
        white-space: nowrap;
        height: 1.25em
    }}
}}

.tile__title--3,
.tile__title--3sm,
.tile__title--3lg {{
    -webkit-line-clamp: 3;
    max-height: 3.75em;
    height: 3.75em;
    font-weight: normal
}}

.tile__title--3sm {{
    font-size: 17.28px;
    font-size: 1.2rem
}}

.tile__title--3lg {{
    font-size: 18.72px;
    font-size: 1.3rem
}}

.tile__title--4 {{
    -webkit-line-clamp: 4;
    max-height: 4.6em;
    height: 4.6em;
    font-weight: normal;
    line-height: 1.15;
    font-size: 1.5em
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .tile__title--4 {{
        font-size: 1.35em
    }}
}}

.is-mobile .tile__title--4 {{
    margin-bottom: 0.5em
}}

.tile__title--min {{
    height: auto
}}

.tile__content {{
    overflow: hidden;
    display: block;
    height: auto
}}

.tile__content br {{
    display: none
}}

.tile__content--sm {{
    height: 4em
}}

.has-foot .tile__content--sm {{
    height: 2.9em
}}

.tile__content--lg {{
    height: 8em
}}

.has-foot .tile__content--lg {{
    height: 6.8em
}}

.tile__link {{
    color: #999
}}

.tile__link img {{
    display: inline-block;
    vertical-align: middle
}}

.tile__icon {{
    max-width: 51px;
    max-height: 47px;
    margin: 0 0 0 0.5em;
    float: right
}}

.tile__icon--left {{
    margin: 0 0.5em 0 0;
    float: left
}}

.tile__icon--pin {{
    background-image: url("/assets/map-pin.png");
    background-size: 25px 35px;
    line-height: 27px;
    height: 35px;
    width: 25px;
    text-align: center;
    display: block;
    font-weight: 600;
    color: #333
}}

.svg .tile__icon--pin {{
    background-image: url("/assets/map-pin.svg")
}}

.tile__more-wrap {{
    display: inline-block;
    position: relative;
    line-height: 1.35em;
    vertical-align: bottom
}}

.tile__more-link {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    margin-right: 1.5em;
    display: block;
    color: #999
}}

.tile__more-link:hover .tile__source {{
    color: inherit
}}

.highlight .tile__source {{
    color: #4495d4
}}

.tile__favicon {{
    max-width: 16px;
    max-height: 16px;
    margin-top: -3px;
    vertical-align: middle;
    margin-right: 0.5em
}}

.tile__icon--ab {{
    position: absolute;
    top: 1em;
    right: 1em
}}

.tile__rule {{
    margin: 0;
    padding: 0;
    height: 0;
    width: auto;
    border: none;
    border-bottom: 1px solid #f3f3f3
}}

.tile__sep {{
    margin: 0 0.5em;
    top: 0.05em;
    height: 0.8em
}}

.tile__count {{
    color: #aaa
}}

.tile__count__icon {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    margin-top: -2px;
    margin-right: 2px;
    font-size: 1.25em;
    display: inline-block;
    vertical-align: middle
}}

.tile__expand {{
    position: absolute;
    width: 100%;
    bottom: 0;
    right: 0;
    left: 0;
    border-top: 1px solid #e6e6e6;
    background-color: #fafafa;
    color: #c9c9c9;
    display: block;
    text-align: center;
    cursor: pointer;
    height: 2em;
    padding: 0
}}

.tile__expand:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    content: "\76";
    line-height: 1.7;
    font-size: 16px;
    display: inline
}}

.is-open .tile__expand:before {{
    content: "\5e"
}}

.highlight .tile__expand,
.tile__expand:hover {{
    color: #595959
}}

.tile__expand:active {{
    background-color: #fff
}}

.is-selected .tile__expand {{
    color: #494949
}}

.tile__expand-icon {{
    position: absolute;
    right: 0;
    top: 0
}}

.tile__expand-icon:before {{
    content: "\2295";
    color: #aaa;
    font-size: 16px;
    line-height: 1
}}

.is-selected .tile__expand-icon:before {{
    content: "\229d"
}}

.tile__check {{
    pointer-events: none;
    font-style: normal;
    position: absolute;
    bottom: 0.5em;
    right: 0.5em;
    font-size: 15px;
    font-size: 1.05rem;
    z-index: 1
}}

.tile__check,
.tile__check:hover {{
    color: #fff;
    text-decoration: none
}}

.tile__check:visited,
.tile__check:visited:hover {{
    color: #c3c3c3
}}

.tile__check:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    content: "\2611";
    line-height: 1
}}

.tile__segment {{
    display: block;
    position: relative;
    width: 100%;
    border-bottom: 1px solid rgba(0, 0, 0, 0.08);
    padding-left: 1em;
    padding-right: 1em;
    box-sizing: border-box;
    min-height: 45px
}}

.tile__segment:hover {{
    text-decoration: none
}}

.tile__segment__title {{
    font-weight: 600;
    color: #333;
    line-height: 1.2
}}

.tile__rating {{
    color: #494949;
    margin-bottom: -0.25em;
    line-height: 1.5
}}

.tile__rating a {{
    vertical-align: top;
    display: inline-block;
    color: inherit
}}

.tile__rating a:hover {{
    color: #4495d4
}}

.tile__rating .review-count {{
    vertical-align: top;
    display: inline-block
}}

.tile__rating .stars {{
    font-size: 14.4px;
    font-size: 1rem;
    top: 1px;
    top: 0.06944rem;
    vertical-align: top;
    line-height: 0
}}

.tile__rating--right .stars {{
    float: right;
    margin-right: 1px
}}

.tile__rating--left .stars {{
    float: left;
    margin-left: -1px
}}

.tile__rating--left .tile__source {{
    float: right
}}

.tile__source {{
    color: #999;
    overflow: hidden;
    display: block;
    height: 1.5em
}}

.tile__price {{
    color: #ddd
}}

.tile__price b {{
    font-weight: 400;
    color: #333;
    position: relative
}}

.tile__phone {{
    display: block
}}

.tile__phone:hover {{
    color: #3a7fb4
}}

.tile__neighborhood {{
    margin-bottom: 0.5em;
    display: block
}}

.tile__hours-today {{
    color: #333;
    padding-bottom: 0.25em
}}

.tile__hours-today__label {{
    margin-right: 0.5em
}}

.tile__hours-today__hours {{
    color: #333
}}

.tile__hours-today__hours.hours-closed,
.tile__hours-today__hours.hours-closed:hover {{
    color: #999;
    text-decoration: none
}}

.tile__hours-full {{
    display: none
}}

.is-showing-hours .tile__hours-full {{
    display: block;
    position: absolute;
    width: 100%;
    bottom: 0;
    border-top: 1px solid #eee;
    padding-top: 1em;
    color: #333
}}

.tile__hours-full__table {{
    width: 100%
}}

.tile__hours-full__table tr {{
    line-height: 1.5em
}}

.tile__hours-full__current td {{
    font-weight: bold
}}

.tile__hours-full__day {{
    width: 15%;
    min-width: 3em;
    position: relative
}}

.tile__hours-full__current .tile__hours-full__day:before {{
    content: "";
    display: block;
    position: absolute;
    top: 0;
    left: -1em;
    width: 4px;
    height: 100%;
    background-color: #de5833
}}

.tile__hours__close {{
    display: block;
    position: absolute;
    right: 0;
    top: 1em;
    color: #333;
    cursor: pointer
}}

.tile__hours__close,
.tile__hours__close:hover {{
    text-decoration: none
}}

.dropdown--directions {{
    height: 30px;
    vertical-align: middle
}}

.dropdown--directions .dropdown__button {{
    height: 30px
}}

.tile__title__sub,
.tile__neighborhood,
.tile__sub,
.tile__sub--2,
.tile__tx {{
    font-size: 12.528px;
    font-size: 0.87rem
}}

.tile--a {{
    background-color: #f7f7f7
}}

.tile--dark {{
    background-color: #424242;
    color: #d9d9d9
}}

.tile--dark .tile__title {{
    color: #fff
}}

.tile--dark .tile__link {{
    color: #fff
}}

.tile--c,
.tile--m {{
    display: inline-block;
    width: 17em
}}

.tile--c--w {{
    display: inline-block;
    width: 20em
}}

.tile--c--n {{
    display: inline-block;
    width: 12em
}}

.tile--e {{
    -webkit-tap-highlight-color: transparent
}}

.tile--f {{
    -moz-transform: rotateY(0) translate3d(0, 0, 50px);
    -webkit-transform: rotateY(0) translate3d(0, 0, 50px);
    transform: rotateY(0) translate3d(0, 0, 50px);
    -moz-transform-origin: center center -50px 50%;
    -ms-transform-origin: center center -50px 50%;
    -webkit-transform-origin: center center -50px 50%;
    transform-origin: center center -50px 50%;
    -webkit-tap-highlight-color: transparent;
    background: none !important
}}

.tile--f.highlight .tile--f__main {{
    border-color: #595959;
    outline: 1px solid #595959
}}

.tile--f.highlight:active .tile--f__main {{
    border-color: #de5833;
    outline-color: #de5833
}}

.tile--f.is-selected .tile--f__alt {{
    border-width: 2px;
    border-color: #595959;
    visibility: visible;
    opacity: 1
}}

.tile--f:before {{
    content: "";
    display: block;
    position: absolute;
    z-index: -1;
    height: 100%;
    width: 100%;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0
}}

.tile--f__main,
.tile--f__alt {{
    -moz-transition: -moz-transform 0.3s ease-in-out 0s;
    -o-transition: -o-transform 0.3s ease-in-out 0s;
    -webkit-transition: -webkit-transform 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: transform 0.3s ease-in-out 0s;
    -moz-backface-visibility: hidden;
    -webkit-backface-visibility: hidden;
    backface-visibility: hidden;
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    background-color: #fff;
    height: 100%;
    z-index: 1
}}

.tile--f__alt {{
    position: absolute;
    visibility: hidden;
    width: 100%;
    opacity: 0;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0
}}

.csstransforms3d .tile--f {{
    border: none;
    -moz-backface-visibility: hidden;
    -webkit-backface-visibility: hidden;
    backface-visibility: hidden;
    -moz-transform: rotateZ(0);
    -ms-transform: rotateZ(0);
    -webkit-transform: rotateZ(0);
    transform: rotateZ(0)
}}

.csstransforms3d .tile--f.is-selected {{
    -moz-box-shadow: none;
    -webkit-box-shadow: none;
    box-shadow: none
}}

.csstransforms3d .tile--f.is-selected .tile--f__main {{
    -moz-transform: rotateZ(0) rotateY(-180deg) translate3d(0, 0, 0);
    -webkit-transform: rotateZ(0) rotateY(-180deg) translate3d(0, 0, 0);
    transform: rotateZ(0) rotateY(-180deg) translate3d(0, 0, 0)
}}

.csstransforms3d .tile--f.is-selected .tile--f__alt {{
    -moz-transform: rotateZ(0) rotateY(0) translate3d(0, 0, 0);
    -webkit-transform: rotateZ(0) rotateY(0) translate3d(0, 0, 0);
    transform: rotateZ(0) rotateY(0) translate3d(0, 0, 0)
}}

.csstransforms3d .tile--f__main,
.csstransforms3d .tile--f__alt {{
    border: 1px solid #e3e3e3;
    border-bottom-color: #cacaca
}}

.csstransforms3d .tile--f__main {{
    -moz-transform: rotateZ(0) rotateY(0) translate3d(0, 0, 0);
    -webkit-transform: rotateZ(0) rotateY(0) translate3d(0, 0, 0);
    transform: rotateZ(0) rotateY(0) translate3d(0, 0, 0)
}}

.csstransforms3d .tile--f__alt {{
    -moz-transform: rotateZ(0) rotateY(180deg) translate3d(0, 0, 0);
    -webkit-transform: rotateZ(0) rotateY(180deg) translate3d(0, 0, 0);
    transform: rotateZ(0) rotateY(180deg) translate3d(0, 0, 0);
    visibility: visible;
    min-height: 100%;
    height: auto;
    opacity: 1;
    top: -1px;
    left: -1px;
    right: -1px;
    bottom: -1px
}}

.tile--m {{
    text-align: center;
    background-color: #e0e0e0;
    color: #666;
    cursor: pointer;
    border: none
}}

.tile--m:hover {{
    background-color: #e8e8e8
}}

.tile--m.is-loading {{
    background-color: #fff;
    color: #fff
}}

.tile--m.is-loading .tile__body,
.tile--m.is-loading .tile--m--mob {{
    visibility: hidden
}}

.tile--m .tile__body {{
    overflow: visible
}}

.tile--m .loader {{
    position: absolute;
    top: 50%;
    left: 50%;
    margin-left: -16px;
    margin-top: -16px
}}

.is-mobile .tile--m .loader:after {{
    display: none
}}

.tile--m__count {{
    display: block;
    font-size: 3em;
    margin-bottom: 0.25em;
    margin-top: 0.25em
}}

.tile--m__title {{
    display: block;
    font-size: 2em;
    margin-bottom: 0;
    margin-top: 1em
}}

.tile--m__subtitle {{
    display: block;
    margin-bottom: 0.5em;
    margin-top: 0.5em
}}

.tile--m__icon {{
    font-style: normal;
    display: block;
    font-size: 1.5em;
    margin-top: 1em
}}

.tile--m__title+.tile--m__icon,
.tile--m__subtitle+.tile--m__icon {{
    margin-top: 0
}}

.tile--m__icon--l {{
    position: absolute;
    margin-top: -0.8em;
    left: 0;
    right: 0;
    top: 50%;
    font-size: 3.8em
}}

.tile--m__icon--l+.tile--m__subtitle {{
    position: absolute;
    margin-bottom: -2.5em;
    bottom: 50%;
    right: 0;
    left: 0
}}

.tile--m--mob {{
    display: none
}}

.is-mobile .tile--m--mob {{
    display: block;
    text-transform: capitalize
}}

.tile--s h1,
.tile--s h2,
.tile--s h3,
.tile--s h4,
.tile--s h5,
.tile--s h6 {{
    font-weight: 600;
    padding-top: 0
}}

.tile--mid {{
    margin-left: auto;
    margin-right: auto
}}

.detail {{
    -moz-box-shadow: inset 0 1px 0 rgba(0, 0, 0, 0.01), inset 0 -1px 0 rgba(0, 0, 0, 0.01);
    -webkit-box-shadow: inset 0 1px 0 rgba(0, 0, 0, 0.01), inset 0 -1px 0 rgba(0, 0, 0, 0.01);
    box-shadow: inset 0 1px 0 rgba(0, 0, 0, 0.01), inset 0 -1px 0 rgba(0, 0, 0, 0.01);
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    background-color: #e5e5e5;
    overflow: hidden;
    clear: both;
    border-top: 1px solid #c7c7c7;
    border-bottom: 1px solid #c7c7c7;
    position: relative;
    top: 0;
    left: 0;
    width: 100%;
    z-index: 1
}}

.is-expanded .detail {{
    position: absolute
}}

.detail.is-hidden {{
    border-color: transparent !important;
    background: none !important;
    display: none;
    padding-bottom: 0;
    padding-top: 0;
    bottom: 0;
    height: 0
}}

.is-mobile .detail {{
    border-top: 2px solid #de5833;
    -webkit-overflow-scrolling: touch;
    -webkit-box-flex: 1;
    overflow: auto;
    padding-top: 0;
    visibility: visible;
    position: fixed;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%;
    z-index: 100;
    opacity: 1
}}

.is-mobile .detail.is-hidden {{
    visibility: hidden;
    z-index: -5;
    opacity: 0
}}

.detail--slider {{
    margin-bottom: -1px
}}

.is-mobile .detail--slider {{
    border-bottom: none
}}

.detail--d {{
    background-color: #e5e5e5;
    border-color: #c7c7c7
}}

.detail--d .detail__body {{
    border-color: #c7c7c7
}}

.detail--l,
.detail--products,
.detail--qa,
.detail--about {{
    background-color: #fafafa;
    border-color: #dbdbdb
}}

.detail--l .detail__body,
.detail--products .detail__body,
.detail--qa .detail__body,
.detail--about .detail__body {{
    border-color: #e3e3e3
}}

.detail--s {{
    background: none
}}

.detail--s .detail__pane {{
    line-height: initial;
    overflow: visible;
    height: auto
}}

.detail--s .detail__media {{
    padding-left: 0;
    padding-right: 1.8em;
    height: auto;
    width: auto
}}

.detail--s .detail__media__img {{
    vertical-align: middle;
    position: relative;
    max-width: 200px
}}

.detail--s .detail__body__content {{
    padding-right: 0;
    display: block
}}

@media only screen and (max-width: 590px) {{
    .detail--s .detail__media {{
        padding-right: 1em
    }}
    .detail--s .detail__body {{
        line-height: inherit;
        padding-right: 0;
        padding-left: 0;
        border-left: none;
        border-top: none
    }}
}}

.detail__wrap {{
    position: relative;
    height: 100%
}}

.is-mobile .detail__wrap {{
    overflow: hidden
}}

.detail__inner {{
    position: static;
    height: 100%;
    width: 100%
}}

.detail--xd {{
    background: #222
}}

.is-mobile .detail--xd {{
    border-top: none
}}

.is-mobile .detail--xd .detail__close {{
    -moz-border-radius: 100%;
    -webkit-border-radius: 100%;
    border-radius: 100%;
    -moz-transform: translateY(-10px);
    -ms-transform: translateY(-10px);
    -webkit-transform: translateY(-10px);
    transform: translateY(-10px);
    -moz-transition: opacity, -moz-transform 0.2s linear;
    -o-transition: opacity, -o-transform 0.2s linear;
    -webkit-transition: opacity, -webkit-transform 0.2s linear;
    transition: opacity, transform 0.2s linear;
    padding: 10px;
    display: block;
    width: 16px;
    height: 16px;
    line-height: 16px;
    text-align: right;
    top: 10px;
    right: 10px;
    background-color: rgba(34, 34, 34, 0.75);
    color: #fff;
    opacity: 0
}}

.is-mobile .detail--xd.has-focus .detail__close {{
    -moz-transform: translateY(0);
    -ms-transform: translateY(0);
    -webkit-transform: translateY(0);
    transform: translateY(0);
    opacity: 1
}}

.is-mobile .detail--xd .detail__controls {{
    display: none
}}

.is-mobile .detail--xd .detail__pane {{
    overflow: hidden
}}

.is-mobile .detail--xd.detail .detail__body {{
    -moz-transition: opacity, -moz-transform 0.2s ease-in-out;
    -o-transition: opacity, -o-transform 0.2s ease-in-out;
    -webkit-transition: opacity, -webkit-transform 0.2s ease-in-out;
    transition: opacity, transform 0.2s ease-in-out;
    -moz-transform: translateY(10px);
    -ms-transform: translateY(10px);
    -webkit-transform: translateY(10px);
    transform: translateY(10px);
    position: absolute;
    display: block;
    top: auto;
    bottom: 0;
    left: 0;
    width: auto;
    height: auto;
    padding: 20px;
    background-color: rgba(34, 34, 34, 0.75);
    z-index: 2;
    opacity: 0
}}

.is-mobile .detail--xd.has-focus .detail__body {{
    -moz-transform: translateY(0);
    -ms-transform: translateY(0);
    -webkit-transform: translateY(0);
    transform: translateY(0);
    opacity: 1
}}

.tile-nav--sm {{
    display: inline-block;
    text-align: center;
    width: 2.5em;
    height: 2.5em;
    line-height: 2.5;
    color: #666;
    background-color: #d1d1d1
}}

.tile-nav--sm:active {{
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s
}}

.detail__close {{
    position: absolute;
    font-size: 0.95em;
    top: 0.25em;
    right: 0;
    padding: 1em;
    cursor: pointer;
    color: #888;
    z-index: 2
}}

.detail__close:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    content: "\58";
    font-size: 16px;
    font-size: 1.11111rem;
    left: 0.05em
}}

.detail__close:hover {{
    color: #333
}}

.detail__close:active {{
    color: #000
}}

.is-mobile .detail__close {{
    top: 0;
    padding: 16px;
    right: 0
}}

.detail__panes {{
    height: 100%;
    left: 0;
    overflow: hidden;
    position: absolute;
    top: 0;
    width: 100%
}}

.detail__pane {{
    overflow: hidden;
    overflow-y: auto;
    position: absolute;
    display: none;
    top: 0;
    left: 0
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail__pane {{
        display: table
    }}
}}

.detail__media,
.detail__body {{
    position: relative
}}

.detail__media {{
    float: left;
    height: 100%;
    width: 55%
}}

@media only screen and (max-width: 590px) {{
    .detail__media {{
        width: 100%
    }}
}}

.is-mobile .detail__media {{
    max-height: 60%;
    width: auto;
    top: 1em;
    float: none
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail__media {{
        display: table-cell;
        vertical-align: middle;
        top: 0;
        width: 50%;
        height: 100%;
        max-height: none
    }}
}}

.detail__media__img-wrapper {{
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0;
    overflow: hidden;
    display: block;
    height: 100%
}}

.detail__media__img-highres,
.detail__media__img-thumbnail {{
    position: absolute;
    top: 0;
    left: 0;
    bottom: 0;
    margin-top: auto;
    margin-bottom: auto;
    max-height: 100%
}}

.is-expanded .detail__media__img-highres,
.is-expanded .detail__media__img-thumbnail {{
    margin: auto;
    right: 0
}}

.detail__media__img-highres {{
    display: none;
    z-index: 2;
    color: transparent
}}

.detail__media__img-highres:-moz-loading {{
    visibility: hidden
}}

.detail__media__img-thumbnail {{
    z-index: 1
}}

.detail__media__placeholder,
.detail__media__img,
.detail__media__vid {{
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0;
    height: 100%;
    width: 100%
}}

.detail__media__img {{
    max-height: 100%;
    height: auto;
    width: auto
}}

.detail__body {{
    overflow: hidden;
    padding-left: 16px;
    vertical-align: middle;
    position: relative
}}

@media only screen and (max-width: 590px) {{
    .detail__body {{
        border-top: 1px solid #e3e3e3;
        border-left: none;
        padding-right: 16px
    }}
}}

.is-mobile .detail__body {{
    border-top: none;
    position: static;
    line-height: initial;
    padding-top: 32px;
    padding-left: 0;
    padding-right: 0;
    bottom: auto;
    top: 60%;
    left: 0;
    right: 0
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail__body {{
        top: 0;
        width: 50%;
        left: 50%;
        height: 100%;
        padding-top: 0;
        display: table-cell;
        vertical-align: middle;
        position: static;
        padding-left: 16px
    }}
}}

.detail__controls {{
    position: absolute;
    line-height: normal;
    bottom: 16px;
    right: 16px;
    left: auto
}}

.detail__controls .tile-nav--sm {{
    font-size: 12px;
    font-size: 0.83333rem
}}

.detail__body__content {{
    padding-right: 15%
}}

.detail--l .detail__media,
.detail--products .detail__media,
.detail--qa .detail__media,
.detail--about .detail__media {{
    background-color: #fff
}}

.detail--l .detail__media:after,
.detail--products .detail__media:after,
.detail--qa .detail__media:after,
.detail--about .detail__media:after {{
    background-color: rgba(0, 0, 0, 0.02);
    pointer-events: none;
    content: ""
}}

.lt-ie9 .detail--l .detail__media,
.detail--l .lt-ie9 .detail__media,
.lt-ie9 .detail--products .detail__media,
.detail--products .lt-ie9 .detail__media,
.lt-ie9 .detail--qa .detail__media,
.detail--qa .lt-ie9 .detail__media,
.lt-ie9 .detail--about .detail__media,
.detail--about .lt-ie9 .detail__media {{
    background: none
}}

.result {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    -webkit-font-smoothing: subpixel-antialiased;
    -webkit-tap-highlight-color: transparent;
    margin-bottom: 0.8em;
    position: relative;
    word-wrap: break-word
}}

.result.highlight {{
    background-color: #f7f7f7
}}

.result__a,
.result__menu__a {{
    color: #333
}}

.result__a {{
    vertical-align: middle;
    display: inline-block;
    max-width: 100%
}}

.result__a:visited {{
    color: #333
}}

@media only screen and (max-width: 590px) {{
    .result__a:visited {{
        color: #888
    }}
}}

.highlight .result__a {{
    color: #3a7fb4
}}

.highlight .result__a:hover {{
    color: #3a7fb4;
    text-decoration: underline
}}

.highlight .result__a:active {{
    color: #333
}}

@media only screen and (min-width: 864px) {{
    .result__a {{
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis
    }}
}}

.result__title--right-badge .result__a {{
    display: inline;
    white-space: normal
}}

.result__body {{
    padding-left: 10px;
    padding-right: 10px;
    padding-top: 0.5em;
    padding-bottom: 0.5em
}}

.result__body.highlight {{
    background-color: #f7f7f7
}}

.result__title {{
    position: static;
    overflow: hidden;
    display: block;
    font-size: 1.31em;
    vertical-align: middle;
    color: #333;
    line-height: 1.1;
    padding: 0;
    margin: 0;
    margin-bottom: 0.2em;
    max-width: 100%
}}

.result__title.result__title--right-badge {{
    overflow: visible;
    white-space: nowrap
}}

.result--url-above-snippet .result__title {{
    margin-bottom: 0
}}

.result__check {{
    cursor: default;
    color: #fff;
    position: absolute;
    right: 100%;
    top: 0.7em;
    margin-right: 1em;
    font-size: 0.8em;
    width: 1em;
    white-space: nowrap;
    speak: none
}}

.result__check,
.result__check:before,
.result__check:after {{
    -moz-transition: all 0.3s ease-in-out 0s;
    -o-transition: all 0.3s ease-in-out 0s;
    -webkit-transition: all 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: all 0.3s ease-in-out 0s
}}

.result__check:before,
.result__check:after {{
    display: inline-block
}}

.result__check:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    content: "\2611";
    line-height: 1;
    float: right
}}

.result__check:hover,
.highlight .result__check:hover {{
    text-decoration: none
}}

.result__check:visited {{
    color: #c3c3c3
}}

.result__check__tt {{
    -moz-border-radius: 1em;
    -webkit-border-radius: 1em;
    border-radius: 1em;
    font-size: 0.76em;
    line-height: 2;
    height: 2em;
    bottom: 2.5em;
    left: -0.95em
}}

.result__check:hover .result__check__tt {{
    -moz-transition-delay: 0.75s;
    -o-transition-delay: 0.75s;
    -webkit-transition-delay: 0.75s;
    transition-delay: 0.75s;
    visibility: visible;
    opacity: 1
}}

.result__snippet {{
    font-size: 0.9176em;
    color: #666;
    line-height: 1.38;
    margin: 0 0 0.1em
}}

.result__snippet b,
.result__snippet strong {{
    color: #333
}}

.result__snippet a {{
    color: inherit
}}

.result--url-above-snippet .result__snippet {{
    margin: 0
}}

.result__sitelinks {{
    display: block;
    height: 1.35em;
    overflow: hidden
}}

.result__sitelinks.result__sitelinks--large {{
    overflow: visible;
    height: auto;
    clear: both;
    padding-top: 15px;
    padding-bottom: 10px
}}

.result__sitelinks.result__sitelinks--large table {{
    width: 100%
}}

.result__sitelinks.result__sitelinks--large td {{
    border-left: 1px solid #e5e5e5;
    padding-left: 20px;
    width: 50%;
    box-sizing: border-box
}}

.sitelinks {{
    clear: both;
    padding-bottom: 20px;
    padding-left: 10px
}}

.sitelinks .result__a:visited {{
    color: #888
}}

.sitelinks__snippet--unbold b,
.sitelinks__snippet--unbold strong {{
    font-weight: normal
}}

.sitelinks__snippet--trim {{
    padding-right: 50px
}}

@media only screen and (max-width: 864px) {{
    .sitelinks__snippet--trim {{
        padding-right: 20px
    }}
}}

.sitelinks__title {{
    font-size: 1.1667em;
    font-weight: bold;
    margin: 0;
    padding: 0
}}

.sitelinks__table {{
    width: 100%
}}

.sitelinks_td {{
    width: 50%;
    padding: 10px 20px;
    cursor: pointer;
    vertical-align: top;
    border-left: solid 1px #e5e5e5
}}

.sitelinks_td.highlight {{
    background-color: #f7f7f7
}}

.result__extras {{
    font-size: 0.9176em;
    position: relative
}}

.result--url-above-snippet .result__extras {{
    height: 1.6em;
    margin: 0 0 0.1em;
    top: 0
}}

.result__extras__url {{
    white-space: nowrap;
    overflow: hidden;
    max-width: 70%
}}

.full-urls .result__extras__url {{
    width: auto;
    padding-right: 4em;
    max-width: 100%
}}

.full-urls .result--url-above-snippet .result__extras__url {{
    padding-right: 0
}}

.full-urls .highlight .result__extras__url {{
    max-width: 70%
}}

.highlight .result__extras__url,
.full-urls .result__extras__url,
.result--ad .result__extras__url {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis
}}

.result__url {{
    color: #888
}}

.result__url:visited {{
    color: #888
}}

.result__url__domain {{
    display: inline
}}

.result__a:hover,
.result__menu__a:hover,
.result__url:hover,
.result__menu:hover {{
    color: #3a7fb4;
    text-decoration: underline
}}

.result__a:active,
.result__menu__a:active,
.result__url:active,
.result__menu:active {{
    text-decoration: none
}}

.result__icon {{
    overflow: hidden;
    display: inline-block;
    vertical-align: middle;
    margin-top: -3px;
    width: 16px;
    margin-right: 0.5em
}}

.result__icon__img,
.result__icon img {{
    vertical-align: middle;
    max-width: 16px;
    margin-top: 0
}}

.result__menu {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    text-align: right;
    color: #888;
    display: none;
    float: right
}}

.result__menu--show {{
    display: inline-block;
    right: 10px;
    bottom: 0.55em
}}

.result__menu--show,
.result__menu--show.result__menu {{
    position: absolute
}}

.full-urls .result__menu--show {{
    width: 4em
}}

.result__extras__url {{
    white-space: nowrap;
    overflow: hidden
}}

.result__url__full {{
    opacity: 0;
    visibility: hidden
}}

.full-urls .result__url__full {{
    opacity: 1;
    visibility: visible
}}

.highlight {{
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    cursor: pointer
}}

.highlight .result__url__full {{
    visibility: visible;
    opacity: 1
}}

.highlight .result__menu {{
    display: inline-block
}}

.highlight .result__menu--show {{
    display: none
}}

.sponsored__url,
.result--ad .result__url,
.sponsored__url b,
.result--ad .result__url b,
.sponsored__sitelink,
.sponsored__sitelink b {{
    color: #888
}}

.sponsored__url:hover,
.result--ad .result__url:hover,
.sponsored__url:hover b,
.result--ad .result__url:hover b,
.sponsored__sitelink:hover,
.sponsored__sitelink:hover b {{
    color: #3a7fb4
}}

.sponsored__url:hover,
.result--ad .result__url:hover,
.sponsored__sitelink:hover {{
    text-decoration: underline
}}

.sponsored__sitelink {{
    white-space: nowrap;
    display: inline-block;
    margin-right: 0.8em
}}

.result__extras .sponsored__sitelink {{
    margin-right: 0
}}

.result__extras .sponsored__sitelink:before {{
    content: "";
    border-left: 1px solid #888;
    display: inline-block;
    margin: 0 0.5em;
    height: 0.7em
}}

.result__sitelinks--large .sponsored__sitelink {{
    display: block;
    color: #333;
    font-size: 14px;
    line-height: 1.7
}}

.result__sitelinks--large .sponsored__sitelink span {{
    border-bottom: 1px solid #e5e5e5
}}

.result__sitelinks--large .sponsored__sitelink b {{
    color: inherit
}}

.result__sitelinks--large .sponsored__sitelink:hover {{
    text-decoration: none
}}

.result__sitelinks--large .sponsored__sitelink:hover span {{
    color: #3a7fb4;
    border-bottom-color: #3a7fb4
}}

.result__sitelinks--large .sponsored__sitelink:before {{
    border-left: none;
    margin: 0;
    height: auto
}}

.result--ad {{
    border: none;
    cursor: default;
    background: none;
    margin-bottom: 0
}}

.result--ad.highlight,
.result--ad.highlight:active {{
    background: none
}}

.has-ad--x2 .result--ad {{
    float: left;
    width: 50%
}}

.has-ad--x2 .result--ad .result__snippet {{
    max-width: 18em;
    min-height: 2.6em
}}

.has-ad--x2 .result--ad .result__extras {{
    height: 1.35em;
    word-wrap: break-word;
    overflow: hidden
}}

.has-ad--x2 .result--ad .result__sitelinks {{
    display: inline
}}

.result--ad.result--ad--small .result__a {{
    font-size: 0.75em
}}

.result--ad.result--ad--small .result__title {{
    margin-bottom: 5px
}}

.result--ad.result--ad--small .result__snippet {{
    font-size: 0.9em;
    margin-bottom: 5px
}}

.result--ad.result--ad--small .result__extras {{
    font-size: 0.9em
}}

.result--ad.result--ad--small .result__sitelinks {{
    display: none
}}

.result--ad.result--ad--small .result--url-above-snippet .result__snippet {{
    margin-bottom: 0
}}

.result--ad.result--ad--small .result--url-above-snippet .result__extras {{
    margin-bottom: 5px;
    height: auto
}}

.result--ad.result--ad--small .result--url-above-snippet .result__extras__url {{
    line-height: 1.1
}}

.result--ad .result__check {{
    display: none
}}

.result--ad .result__extras:after {{
    content: "";
    display: table;
    clear: both
}}

.result--ad .result__extras__url {{
    display: inline-block;
    max-width: 19em;
    width: auto;
    padding-right: 0
}}

.result--ad .sponsored__info {{
    width: auto;
    position: absolute;
    bottom: 0;
    right: 0
}}

.result--more {{
    cursor: default;
    position: relative;
    margin-bottom: 0;
    bottom: 0;
    right: 0;
    left: 0
}}

.result--more__btn {{
    line-height: 2.8;
    font-weight: 600;
    font-size: 1.1em
}}

.result--load-btn {{
    background: none !important;
    border-top: 1px solid #ddd;
    display: block;
    text-align: center;
    line-height: 3.2;
    margin-top: 1.3em;
    margin-right: 10px;
    margin-left: 10px;
    margin-bottom: 0.8em
}}

.result--load-btn .loader {{
    vertical-align: middle
}}

.result--load-btn.is-loading .loader {{
    display: inline-block
}}

.result--sep {{
    cursor: default;
    margin: 1em auto 1em;
    padding-top: 0;
    padding-bottom: 0;
    line-height: 1.5;
    height: 1.5em
}}

.result--sep--hr:before {{
    content: "";
    position: absolute;
    top: 0.75em;
    left: 10px;
    right: 20px;
    background-color: #ededed;
    display: block;
    height: 1px
}}

.result--sep--hr.has-pagenum:before {{
    left: 30px
}}

.result--url-above-snippet .result__menu {{
    float: none;
    text-align: left;
    margin-left: 0.5em;
    width: auto
}}

.result__badge,
.result__type,
.result__pagenum {{
    padding-top: 1px;
    padding-right: 0.35em;
    padding-left: 0.35em;
    margin-top: -0.2em;
    line-height: 1.3;
    font-weight: 600;
    top: -1px
}}

.result__badge {{
    top: 0.45em;
    float: left;
    margin-left: 0;
    margin-right: 0.6em;
    text-transform: uppercase
}}

.result__type,
.result__pagenum {{
    background-color: #d0d0d0;
    color: #666
}}

.result__pagenum {{
    margin-top: 0;
    top: 0
}}

.result__pagenum--side {{
    padding: 0;
    background-color: transparent
}}

.deep_image {{
    height: 75px;
    width: 75px;
    position: absolute;
    right: -75px;
    background-repeat: no-repeat;
    background-position: top right;
    background-size: auto 75px
}}

@media only screen and (max-width: 590px) {{
    .result,
    .result__check,
    .result__check:before {{
        -moz-transition: none 0.3s ease-in-out 0s;
        -o-transition: none 0.3s ease-in-out 0s;
        -webkit-transition: none 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: none 0.3s ease-in-out 0s
    }}
    .result__check {{
        display: none
    }}
    .result__title {{
        margin-bottom: 0.25em
    }}
    .sitelinks__title {{
        font-size: 1em
    }}
    .result--ad .result__a {{
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis
    }}
    .result--ad .result__snippet {{
        overflow: hidden;
        max-height: 2.6em
    }}
    .result--ad .sponsored__info {{
        font-size: 0.8em;
        padding-bottom: 0.125em
    }}
}}

.search-filter__icon,
.nav-menu__item__icon--date,
.nav-menu__item__icon--region {{
    display: block;
    width: 20px;
    height: 20px;
    opacity: 0.35
}}

.search-filter__icon:hover,
.nav-menu__item__icon--date:hover,
.nav-menu__item__icon--region:hover,
.is-active .search-filter__icon,
.is-active .nav-menu__item__icon--date,
.is-active .nav-menu__item__icon--region {{
    opacity: 0.8
}}

.search-filter--region .search-filter__icon,
.nav-menu__item__icon--region {{
    background: url("/assets/icon_region.svg") center no-repeat
}}

.dark-header .search-filter--region .search-filter__icon,
.search-filter--region .dark-header .search-filter__icon,
.dark-header .nav-menu__item__icon--region,
.dark-bg .search-filter--region .search-filter__icon,
.search-filter--region .dark-bg .search-filter__icon,
.dark-bg .nav-menu__item__icon--region {{
    background-image: url("/assets/icon_region_dark.svg")
}}

.nav-menu__item__icon--date {{
    background: url("/assets/icon_date.svg") center no-repeat
}}

.dark-header .nav-menu__item__icon--date,
.dark-bg .nav-menu__item__icon--date {{
    background-image: url("/assets/icon_date_dark.svg")
}}

.search__filters {{
    position: absolute;
    top: 0;
    left: 100%;
    margin-left: -50px;
    right: 0;
    width: 300px
}}

@media only screen and (max-width: 1079px) {{
    .search__filters {{
        margin-left: -10px
    }}
}}

.search-filter {{
    position: relative;
    float: left;
    height: 38px;
    line-height: 38px;
    margin-left: 14px
}}

.search-filter.search-filter--date {{
    margin-left: 20px;
    cursor: pointer
}}

.search-filter--date .dropdown {{
    font-size: 13px
}}

.search-filter--date .dropdown__button:after {{
    margin-top: -6px
}}

.search-filter--date:after {{
    content: "";
    display: block;
    position: absolute;
    left: -10px;
    height: 20px;
    top: 9px;
    width: 1px;
    background: rgba(0, 0, 0, 0.1)
}}

.search-filter__icon {{
    text-decoration: none;
    margin-top: 9px;
    cursor: pointer
}}

.search-filter--region .search-filter__icon .no-region {{
    display: none
}}

.search-filter--region.is-active .search-filter__icon,
.dark-header .search-filter--region.is-active .search-filter__icon {{
    background: none;
    opacity: 1
}}

.search-filter__switch {{
    position: relative;
    float: left;
    margin-top: 11px;
    margin-right: 10px
}}

.modal--search-filter.modal--popout {{
    bottom: auto;
    top: 45px
}}

.modal--search-filter .modal__box {{
    width: 220px !important
}}

.modal--search-filter .modal__box {{
    text-align: left
}}

.modal--search-filter .modal__header {{
    padding: 0.75em 1em
}}

.modal--search-filter .modal__header__clear {{
    float: right;
    cursor: pointer
}}

.modal--search-filter .modal__body {{
    padding: 0.5em
}}

.modal--search-filter .modal__list__link {{
    padding: 0.5em
}}

.modal--date-filter {{
    left: -1em;
    right: auto
}}

.modal--region-filter.modal--popout {{
    right: 0;
    left: -220px
}}

.modal--region-filter .modal__body {{
    padding: 0
}}

.modal--region-filter .modal__body__input {{
    padding: 0.5em
}}

.modal--region-filter .frm__input {{
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    width: 100%;
    height: 32px;
    line-height: 32px;
    padding-left: 0.5em
}}

.modal--region-filter .frm__input::-ms-clear {{
    display: none
}}

.modal--region-filter .modal__list {{
    height: 300px;
    overflow: auto
}}

.modal--region-filter .modal__list__item {{
    margin: 0 0.5em
}}

.modal--region-filter .modal__list__divider {{
    margin: 0.5em 0;
    height: 1px;
    background: rgba(0, 0, 0, 0.1)
}}

.modal--region-filter .modal__list__link {{
    line-height: 21px;
    height: 20px
}}

.modal--region-filter .modal__list__link:hover,
.modal--region-filter .modal__list__link:active {{
    background-color: transparent
}}

.modal--region-filter .modal__list__link.is-highlighted {{
    background-color: #f7f7f7
}}

.modal--region-filter .modal__list__link .region-flag__wrap {{
    margin-right: 0.5em
}}

.nav-menu__item__icon--date {{
    float: left
}}

.nav-menu__item__icon--region .no-region {{
    display: none
}}

a.nav-menu__filter,
a.nav-menu__filter:hover,
a.nav-menu__filter:active {{
    color: #888
}}

a.nav-menu__filter.is-active,
a.nav-menu__filter:hover.is-active,
a.nav-menu__filter:active.is-active {{
    color: #333
}}

@media only screen and (max-width: 1008px) {{
    .search-filter--date {{
        display: none
    }}
}}

@media only screen and (min-width: 1008px) {{
    .nav-menu--filters {{
        display: none
    }}
}}

@media only screen and (max-width: 1200px) {{
    .set-align-center .search-filter--date {{
        display: none
    }}
}}

@media only screen and (min-width: 1200px) {{
    .set-align-center .nav-menu--filters {{
        display: none
    }}
}}

.c-info,
.c-base {{
    line-height: 1.37;
    display: block
}}

.c-info__title,
.c-icon__title,
.c-list__title,
.c-base__title {{
    margin-top: 0;
    margin-bottom: 0;
    padding-bottom: 0;
    padding-top: 0;
    display: block;
    overflow: hidden;
    font-size: 1.31em;
    font-weight: 600;
    line-height: 1.2;
    color: #333
}}

.c-info__sub,
.c-info__title__sub,
.c-list__sub,
.c-base__sub {{
    font-size: 14px;
    font-size: 0.97222rem;
    margin-bottom: 0;
    margin-top: 0;
    padding-bottom: 0;
    padding-top: 3px;
    display: block;
    font-weight: normal;
    line-height: 1.2;
    color: #888
}}

.c-info__content,
.c-list__content,
.c-base__content {{
    margin-top: 0.75em
}}

.c-info__links,
.c-icon__links,
.c-list__links,
.c-base__links {{
    margin-top: 0.75em;
    display: block;
    clear: both
}}

.c-info__link,
.c-icon__link,
.c-list__link,
.c-base__link {{
    font-size: 0.9176em;
    color: #888
}}

.c-info__link:hover,
.c-icon__link:hover,
.c-list__link:hover,
.c-base__link:hover {{
    color: #4495d4
}}

.c-info__link--more,
.c-icon__link--more,
.c-list__link--more,
.c-base__link--more {{
    font-size: 0.9176em;
    color: #aaa
}}

.c-info__link--more a,
.c-icon__link--more a,
.c-list__link--more a,
.c-base__link--more a {{
    color: #888
}}

.c-info__link--more a:hover,
.c-icon__link--more a:hover,
.c-list__link--more a:hover,
.c-base__link--more a:hover {{
    color: #4495d4
}}

.c-base__img-wrap {{
    margin: 0 0 0.5em 0.5em;
    position: relative;
    text-align: center;
    overflow: hidden;
    float: right
}}

.c-base__img {{
    min-width: 1px;
    display: inline;
    position: relative;
    vertical-align: middle
}}

.c-info__title--long {{
    font-weight: 400
}}

.c-info__title--long b {{
    font-weight: 600
}}

.c-info__title__sub {{
    padding-left: 1em;
    display: inline-block; !important
    text-transform: capitalize
    padding-left: 1em;
    display: inline-block;
    text-transform: capitalize;
    font-size: 14px;
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .c-info__title__sub {{
        padding-left: 0;
        display: block
    }}
}}

@media only screen and (max-width: 590px) {{
    .c-info__title__sub {{
        padding-left: 0;
        display: block
    }}
}}

.c-info__body {{
    display: block;
    overflow: visible
}}

.c-info__img-wrap+.c-info__body {{
    display: table-cell;
    vertical-align: middle;
    padding-left: 2em
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .c-info__img-wrap+.c-info__body {{
        padding-bottom: 6px;
        padding-top: 6px
    }}
}}

.c-info__img-wrap {{
    position: relative;
    overflow: hidden;
    text-align: center;
    display: table-cell;
    vertical-align: top;
    height: 120px;
    line-height: 120px;
    max-width: 180px;
    margin-left: 0;
    margin-right: 0;
    top: 0;
    left: 0
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .c-info__img-wrap {{
        height: 150px;
        line-height: 150px;
        max-width: 220px
    }}
    .c-info__img-wrap img {{
        max-height: 150px
    }}
}}

@media only screen and (max-width: 590px) {{
    .c-info__img-wrap {{
        left: 0;
        float: right;
        margin-left: 0.25em
    }}
    .c-info__img-wrap+.c-info__body {{
        display: block;
        overflow: hidden;
        padding-left: 0
    }}
}}

.c-info__img-wrap--clip .c-info__img-wrap__in {{
    float: left;
    display: block;
    position: relative;
    text-align: center;
    left: 50%
}}

.c-info__img-wrap--clip .c-info__img {{
    margin-bottom: -1px;
    max-width: none;
    right: 50%
}}

.c-info__img-wrap--tile .c-info__img-wrap__in {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    height: 120px;
    border: 1px solid #dcdcdc;
    background-color: #fff;
    padding: 0 15px;
    display: block
}}

.c-info__img-wrap--tile .c-info__img {{
    margin-top: -3px;
    max-width: 150px;
    max-height: 90px
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .c-info__img-wrap--tile .c-info__img {{
        max-width: 190px;
        max-height: 120px
    }}
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .c-info__img-wrap--tile .c-info__img-wrap__in {{
        height: 150px
    }}
}}

.c-info__img {{
    min-width: 1px;
    max-width: 180px;
    max-height: 120px;
    display: inline;
    vertical-align: middle;
    position: relative
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .c-info__img {{
        max-width: 220px;
        max-height: 150px
    }}
}}

.c-info__content.chomp {{
    # max-height: 4.25em
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .c-info__content.chomp {{
        line-height: 1.35;
        # max-height: 5.5em
    }}
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .has-sub+.c-info__content.chomp {{
        # max-height: 4.25em
    }}
}}

.is-mobile .c-info__link--chomp:after {{
    display: none
}}

.c-info__links {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    max-width: 100%
}}

.c-info__links--attr {{
    overflow: visible
}}

.is-mobile .c-info--cw.cw.has-aux {{
    padding-bottom: 4em
}}

.is-mobile .c-info--cw.cw.has-aux+.zci__aux {{
    bottom: 4em
}}

.is-mobile .c-info {{
    padding-bottom: 4em
}}

.is-mobile .has-aux.cw .c-info {{
    margin-bottom: -0.5em;
    padding-bottom: 0
}}

.is-mobile .c-info__body {{
    position: static
}}

.is-mobile .c-info__img-wrap+.c-info__body {{
    display: block;
    padding: 0
}}

.is-mobile .is-expanded .c-info__body {{
    padding-top: 0;
    padding-bottom: 0.75em;
    overflow: visible
}}

.is-mobile .is-expanded .c-info__content {{
    top: 0.75em;
    position: relative;
    display: inline
}}

.is-mobile .is-expanded .c-info__img-wrap {{
    margin-bottom: -0.75em
}}

.is-mobile .is-expanded .c-info--body {{
    padding-bottom: 4.75em
}}

.is-mobile .c-info__img-wrap {{
    float: right;
    left: 0;
    height: 90px;
    line-height: 90px;
    margin: 0 0 0.25em 0.5em
}}

.is-mobile .c-info__img {{
    max-height: 90px
}}

.is-mobile .c-info__img-wrap--tile {{
    max-width: 120px
}}

.is-mobile .c-info__img-wrap--tile .c-info__img-wrap__in {{
    height: 90px;
    padding: 0 8px
}}

.is-mobile .c-info__img-wrap--tile .c-info__img {{
    max-width: 100%;
    max-height: 74px
}}

.is-mobile .is-expanded .c-info__img-wrap--tile {{
    float: right;
    margin-bottom: 0
}}

.is-mobile .c-info__links {{
    display: block;
    text-align: center;
    position: absolute;
    right: 1.25em;
    left: 1.25em;
    padding-top: 0;
    bottom: 0;
    margin: 0 -1px
}}

.is-mobile .c-info__link {{
    -webkit-appearance: none;
    -moz-appearance: none;
    -ms-appearance: none;
    -o-appearance: none;
    appearance: none;
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s;
    -moz-border-radius: 0.25em;
    -webkit-border-radius: 0.25em;
    border-radius: 0.25em;
    outline: none !important;
    text-decoration: none !important;
    background-color: #fafafa;
    border: 1px solid #ddd;
    cursor: pointer;
    -webkit-user-select: none;
    -khtml-user-select: none;
    -moz-user-select: -moz-none;
    -ms-user-select: none;
    user-select: none;
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    display: inline-block;
    vertical-align: middle;
    font-weight: 600;
    width: 100%;
    padding: 0;
    margin: 0 auto;
    overflow: hidden;
    text-overflow: ellipsis;
    font-size: 1em;
    line-height: 3
}}

.is-mobile .c-info__link,
.is-mobile .c-info__link:hover {{
    color: #333
}}

.is-mobile .c-info__link--chomp {{
    margin-right: 4%;
    display: none;
    width: 39%
}}

.is-mobile .c-info__link--chomp.can-expand {{
    display: inline-block
}}

.is-mobile .c-info__link--chomp .chomp--link__icn {{
    display: none
}}

.is-mobile .c-info__link--chomp .chomp--link__mr,
.is-mobile .c-info__link--chomp .chomp--link__mr:hover,
.is-mobile .c-info__link--chomp .chomp--link__ls,
.is-mobile .c-info__link--chomp .chomp--link__ls:hover {{
    color: inherit
}}

.is-mobile .c-info__link--chomp.can-expand+.c-info__link {{
    width: 57%
}}

.is-mobile .has-aux .c-info__links {{
    bottom: 1em
}}

.is-mobile .c-info__link--url,
.is-mobile .c-info__link--more {{
    display: none !important
}}

.c-icon {{
    vertical-align: middle;
    line-height: 1.37
}}

.c-icon--head,
.c-icon__head {{
    display: table;
    width: 100%
}}

.c-icon__img-wrap,
.c-icon__badge {{
    overflow: hidden;
    position: relative;
    vertical-align: middle;
    display: table-cell;
    text-align: center
}}

.c-icon__img-wrap--sm {{
    width: 40px;
    height: 40px
}}

.c-icon__img-wrap--sm:before {{
    content: '';
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    border: 2px solid #000;
    position: absolute;
    opacity: 0.15;
    border-radius: 100%;
    z-index: 1
}}

.c-icon__img-wrap--md {{
    width: 50px;
    height: 50px
}}

.c-icon__img-wrap--md:before {{
    content: '';
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    border: 2px solid #000;
    position: absolute;
    opacity: 0.15;
    border-radius: 100%;
    z-index: 1
}}

.c-icon__img-wrap--lg {{
    width: 60px;
    height: 60px
}}

.c-icon__img-wrap--lg:before {{
    content: '';
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    border: 2px solid #000;
    position: absolute;
    opacity: 0.15;
    border-radius: 100%;
    z-index: 1
}}

.c-icon__img {{
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
    vertical-align: middle;
    display: inline-block;
    max-height: 100%;
    max-width: 100%;
    height: auto;
    width: auto
}}

.c-icon__badge {{
    -moz-border-radius: 3px;
    -webkit-border-radius: 3px;
    border-radius: 3px;
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    line-height: 40px;
    max-width: 40px;
    height: 40px;
    width: 40px;
    color: #fff
}}

.c-icon__badge--sm {{
    font-size: 14px
}}

.c-icon__badge--md {{
    font-size: 21px
}}

.c-icon__badge--lg {{
    font-size: 30px
}}

.c-icon__body {{
    vertical-align: middle;
    display: table-cell
}}

.c-icon__img-wrap+.c-icon__body,
.c-icon__badge+.c-icon__body {{
    padding-left: 1em
}}

.c-icon__title {{
    font-size: 1.18em
}}

.c-icon__title--lg {{
    font-size: 1.32em
}}

.c-icon__sub {{
    font-size: 0.9em;
    margin-bottom: 0;
    margin-top: 0;
    padding-bottom: 0;
    padding-top: 0;
    display: block;
    font-weight: normal;
    line-height: 1.37;
    color: #999
}}

.c-icon__sub a {{
    color: inherit
}}

.c-icon__sub a:hover {{
    color: #4495d4
}}

.c-icon__content {{
    font-size: 1.18em;
    display: block;
    color: #333;
    padding: 1em 0 0
}}

.c-list {{
    line-height: 1.37;
    display: block
}}

.c-list__content.chomp {{
    max-height: 8.5em
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .c-list__content.chomp {{
        max-height: 13em
    }}
}}

.c-list__items {{
    border-top: 1px solid #d0d0d0;
    line-height: 1.3;
    list-style: none;
    padding: 0;
    margin: 0
}}

.c-list__item {{
    word-wrap: break-word;
    border-bottom: 1px solid #d0d0d0;
    padding: 0.35em 0.35em 0.35em 0
}}

.c-score__team__logo__ph,
.c-score__win-marker,
.c-score__clock__icon {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale
}}

.c-score {{
    font-size: 14px;
    font-size: 0.97222rem;
    min-width: 16em;
    font-weight: normal;
    display: block;
    color: #333;
    padding: 0
}}

.is-mobile .c-score {{
    -webkit-overflow-scrolling: touch;
    overflow-x: auto;
    max-width: 100%
}}

.is-mobile .c-score.no-score {{
    overflow: hidden
}}

.c-score__opt {{
    -moz-transition: all 0.3s ease-out 0s;
    -o-transition: all 0.3s ease-out 0s;
    -webkit-transition: all 0.3s ease-out;
    -webkit-transition-delay: 0s;
    transition: all 0.3s ease-out 0s;
    display: inline-block;
    vertical-align: top;
    overflow: hidden;
    max-width: 0;
    width: auto
}}

.lt-ie9 .c-score__opt {{
    width: 0
}}

.is-expanded .c-score__opt,
.is-selected .c-score__opt {{
    -moz-transition: all 0.3s ease-in 0s;
    -o-transition: all 0.3s ease-in 0s;
    -webkit-transition: all 0.3s ease-in;
    -webkit-transition-delay: 0s;
    transition: all 0.3s ease-in 0s;
    width: auto;
    max-width: 600px;
    visibility: visible;
    opacity: 1
}}

.is-mobile .c-score__opt {{
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s
}}

@media only screen and (max-width: 425px) {{
    .c-score__opt--min-xs {{
        display: none
    }}
}}

.c-score__sequence,
.c-score__totals {{
    display: inline-block;
    vertical-align: top
}}

.is-mobile .c-score__sequence {{
    padding-right: 4em;
    max-width: none
}}

.is-mobile .c-score__totals {{
    margin-left: -100%;
    float: right
}}

.c-score__head,
.c-score__foot {{
    color: #888;
    padding: 0 1.25em;
    line-height: 45px;
    height: 45px;
    white-space: nowrap;
    overflow: hidden;
    clear: both
}}

.is-mobile .c-score__head,
.is-mobile .c-score__foot {{
    padding: 0 0.75em
}}

.c-score__foot {{
    padding-right: 2.5em
}}

.c-score__head {{
    margin-bottom: -1px
}}

.is-mobile .has-score .c-score__head {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    margin-bottom: 0;
    min-width: 100%;
    float: left
}}

.c-score__head__date,
.c-score__venue {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    display: inline-block;
    max-width: 12em
}}

.is-selected .has-score .c-score__head__date,
.is-selected .has-score .c-score__venue {{
    margin-right: 1.25em;
    width: 12em
}}

.is-mobile .has-score .c-score__head__date,
.is-mobile .has-score .c-score__venue {{
    margin-right: 0.75em
}}

.has-score .c-score__head__date {{
    margin-right: 1.25em;
    width: 12em;
    padding-right: 1px
}}

.c-score__venue {{
    -moz-transition: all 0.3s ease-in 0s;
    -o-transition: all 0.3s ease-in 0s;
    -webkit-transition: all 0.3s ease-in;
    -webkit-transition-delay: 0s;
    transition: all 0.3s ease-in 0s
}}

.has-score .c-score__venue {{
    max-width: 15.75em;
    width: 15.75em;
    padding-right: 1px
}}

.is-selected .c-score--nfl.has-score .c-score__venue {{
    max-width: 22.5em;
    width: 22.5em
}}

.is-mobile .c-score__venue {{
    overflow: visible
}}

.c-score__head__time {{
    float: right
}}

@media only screen and (max-width: 501.5px) {{
    .c-score__venue__name {{
        display: none
    }}
}}

.c-score__foot {{
    border-top: 1px solid #ededed
}}

.is-mobile .no-score .c-score__foot {{
    display: none
}}

.c-score__foot__main {{
    height: 45px;
    display: block;
    overflow: hidden
}}

.c-score__foot__main a {{
    color: inherit
}}

.c-score__foot__more {{
    visibility: hidden;
    opacity: 0
}}

.is-mobile .c-score__foot__more {{
    visibility: visible;
    float: right;
    opacity: 1
}}

.c-score__foot__info {{
    padding: 0 0.5em
}}

.is-mobile .c-score__foot__info {{
    padding: 0 0 0 1.25em
}}

.is-mobile .c-score__foot__info:first-child {{
    padding-left: 0
}}

.c-score__foot__time {{
    float: right;
    color: #5b9e4d
}}

.c-score__foot-icon {{
    padding-right: 1.25em;
    line-height: 45px;
    height: 45px;
    display: block;
    bottom: 0;
    top: auto
}}

.is-mobile .c-score__foot-icon {{
    display: none
}}

.c-score__line {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    border-top: 1px solid #ededed;
    line-height: 54px;
    padding: 0 1.25em;
    height: 54px;
    white-space: nowrap;
    position: relative;
    display: block;
    clear: both
}}

.is-mobile .c-score__line {{
    padding: 0 0.75em;
    min-width: 100%;
    display: block;
    float: left
}}

.is-mobile .no-score .c-score__line {{
    display: inline-block;
    min-width: initial;
    width: 50%
}}

.is-mobile .no-score .c-score__line:before,
.is-mobile .no-score .c-score__line:after {{
    display: block
}}

.c-score__line--vs1:before,
.c-score__line--vs1:after {{
    content: "";
    display: none;
    height: 16px;
    border-right: 1px solid #ededed;
    position: absolute;
    right: 0
}}

.c-score__line--vs1:before {{
    top: 0
}}

.c-score__line--vs1:after {{
    bottom: 0
}}

.is-mobile .no-score .c-score__line--vs2 {{
    clear: none
}}

.c-score__line--vs2:before {{
    content: attr(data-vs);
    display: none;
    line-height: 1;
    font-size: 14px;
    position: absolute;
    text-align: center;
    margin-top: -0.5em;
    width: 2em;
    left: -1em;
    top: 50%
}}

.is-mobile .no-score .c-score__line--vs2 {{
    padding-left: 1.25em
}}

.c-score__line__name {{
    display: inline-block;
    width: 12em;
    height: 100%
}}

.has-score .c-score__line__name {{
    border-right: 1px solid #ededed;
    margin-right: 1.25em
}}

.is-mobile .no-score .c-score__line__name {{
    width: 90%
}}

.is-mobile .has-score .c-score__line__name {{
    margin-right: 0.75em
}}

.c-score__item {{
    display: inline-block;
    text-align: center;
    width: 2em
}}

.c-score__head__total {{
    display: inline-block;
    vertical-align: top;
    text-align: center;
    width: 4em
}}

.c-score__line__total {{
    display: inline-block;
    vertical-align: top;
    text-align: center;
    width: 2.66667em;
    font-size: 1.5em;
    font-weight: 600
}}

.c-score__team {{
    position: relative;
    display: inline-block;
    vertical-align: middle;
    width: 100%
}}

.c-score__team__logo {{
    margin-top: -2px;
    margin-left: -2px;
    margin-right: 1em;
    height: 40px;
    width: 40px;
    text-align: center;
    display: block;
    float: left
}}

.is-mobile .c-score__team__logo {{
    margin-right: 0.5em
}}

.c-score__team__logo__ph {{
    display: inline-block;
    vertical-align: baseline;
    font-size: 1.8em;
    line-height: 1.4
}}

.c-score__team__img {{
    display: inline-block;
    vertical-align: baseline;
    max-height: 100%;
    max-width: 100%
}}

.c-score__team__name {{
    margin-top: 3px;
    line-height: 1;
    display: block;
    overflow: hidden
}}

.c-score__team__name.has-status {{
    margin-right: 40px
}}

.is-mobile .has-score.c-score--mlb .c-score__team__name {{
    display: none
}}

.c-score__team__short {{
    display: none;
    overflow: hidden;
    font-weight: 600;
    line-height: 40px
}}

.c-score__team__market {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    max-width: 100%;
    display: block
}}

.c-score__team__nick {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    font-weight: 600;
    font-size: 1.1667em;
    max-width: 100%;
    display: block
}}

.c-score__team__link {{
    display: block;
    color: inherit
}}

.c-score__team__status {{
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    color: #333;
    background-color: #e5e5e5;
    text-align: center;
    font-size: 16px;
    line-height: 28px;
    height: 28px;
    width: 28px;
    position: absolute;
    display: block;
    margin: auto;
    right: 12px;
    bottom: 2px;
    top: 0
}}

.c-score__team__status:after {{
    content: "\25c0";
    color: #e5e5e5;
    position: absolute;
    width: 10px;
    right: 100%;
    bottom: 0;
    top: 0
}}

.c-score__is-current {{
    border: 1px solid #c3c3c3;
    display: inline-block;
    line-height: 1.1;
    padding: 20% 0;
    width: 90%
}}

.c-score__win-marker {{
    display: inline-block;
    vertical-align: top;
    position: relative;
    font-size: 16px;
    left: 2px;
    width: 0
}}

.c-score__win-marker:before {{
    content: "\25c0"
}}

.c-score__status {{
    text-transform: capitalize;
    color: #de5833
}}

.c-score__state {{
    margin-right: 12px
}}

.c-score__clock {{
    color: #5b9e4d
}}

.c-score__clock__icon {{
    display: inline-block;
    vertical-align: middle;
    position: relative;
    font-size: 16px;
    line-height: 0;
    top: -1px
}}

@media only screen and (max-width: 425px) {{
    .is-mobile .c-score .c-score__sequence {{
        display: none
    }}
}}

@media only screen and (max-width: 864px) {{
    .is-mobile .has-score.c-score--mlb .c-score__team__name {{
        display: block
    }}
    .is-mobile .has-score.c-score--mlb .c-score__team__short {{
        display: none
    }}
    .is-mobile .has-score.c-score--mlb .c-score__head__date,
    .is-mobile .has-score.c-score--mlb .c-score__venue,
    .is-mobile .has-score.c-score--mlb .c-score__line__name {{
        width: 12em
    }}
    .is-mobile .has-score.c-score--mlb .c-score__venue__name {{
        display: inline
    }}
}}

@media only screen and (max-width: 590px) {{
    .is-mobile .has-score.c-score--mlb .c-score__team__name {{
        display: none
    }}
    .is-mobile .has-score.c-score--mlb .c-score__team__short {{
        display: block
    }}
    .is-mobile .has-score.c-score--mlb .c-score__head__date,
    .is-mobile .has-score.c-score--mlb .c-score__venue,
    .is-mobile .has-score.c-score--mlb .c-score__line__name {{
        width: 6em
    }}
}}

@media only screen and (max-width: 501.5px) {{
    .is-mobile .has-score.c-score--mlb .c-score__venue__name {{
        display: none
    }}
}}

@media only screen and (max-width: 425px) {{
    .is-mobile .has-score.c-score--mlb .c-score__team__name {{
        display: block
    }}
    .is-mobile .has-score.c-score--mlb .c-score__team__short {{
        display: none
    }}
    .is-mobile .has-score.c-score--mlb .c-score__head__date,
    .is-mobile .has-score.c-score--mlb .c-score__venue,
    .is-mobile .has-score.c-score--mlb .c-score__line__name {{
        width: 12em
    }}
    .is-mobile .has-score.c-score--mlb .c-score__venue {{
        max-width: 6em;
        overflow: hidden
    }}
}}

.in-progress.has-score.c-score--nfl .c-score__head__date,
.in-progress.has-score.c-score--nfl .c-score__venue,
.in-progress.has-score.c-score--nfl .c-score__line__name {{
    max-width: 15em;
    width: 15em
}}

@media only screen and (min-width: 501.5px) {{
    .is-mobile .c-score--nfl .c-score__head__date,
    .is-mobile .c-score--nfl .c-score__venue,
    .is-mobile .c-score--nfl .c-score__line__name {{
        max-width: 20em;
        width: 20em
    }}
    .is-mobile .c-score--nfl .c-score__team__name {{
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis;
        margin-top: 0;
        font-weight: 600;
        font-size: 1.1667em;
        line-height: 40px
    }}
    .is-mobile .c-score--nfl .c-score__team__market,
    .is-mobile .c-score--nfl .c-score__team__nick {{
        font-size: 1em;
        display: inline
    }}
}}

.is-mobile .c-score--mlb .c-score__sequence {{
    padding-right: 8.75em
}}

.is-mobile .c-score--nfl .c-score__sequence {{
    padding-right: 6.75em
}}

@media only screen and (max-width: 501.5px) {{
    .is-mobile .c-score--mlb .c-score__sequence {{
        padding-right: 7.25em
    }}
}}

@media only screen and (max-width: 501.5px) {{
    .c-score__item--inning {{
        width: 1.25em
    }}
}}

.c-product {{
    display: block
}}

.c-product__pane {{
    min-height: 160px;
    line-height: 160px
}}

.c-product__media {{
    border-right: 1px solid #dbdbdb;
    background-color: #fff;
    position: relative;
    padding-right: 1.8em;
    margin-right: 1.8em;
    line-height: 160px;
    min-width: 60px;
    height: auto;
    width: auto;
    float: left
}}

.c-product__media:after {{
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0;
    background-color: rgba(0, 0, 0, 0.044);
    pointer-events: none;
    content: ""
}}

.c-product__media__img {{
    vertical-align: middle;
    position: relative;
    max-width: 200px;
    max-height: 160px;
    height: auto;
    width: auto
}}

.c-product__body {{
    line-height: normal;
    position: relative;
    border-left: none;
    padding-left: 0
}}

.c-product__body-wrap {{
    display: block;
    overflow: hidden;
    vertical-align: middle
}}

.c-product__body-content {{
    display: inline-block;
    vertical-align: middle;
    line-height: 1.2
}}

.c-product__rating {{
    display: inline-block;
    margin-bottom: 0;
    line-height: 1.8
}}

.c-product__title {{
    padding-bottom: 0;
    padding-top: 0;
    color: #333
}}

.c-product__subtitle {{
    max-height: 2.7em
}}

.c-product__subtitle--nm {{
    max-height: none
}}

.c-product__subtitle,
.c-product__desc {{
    padding: 0.25em 0;
    color: #888;
    font-size: 0.9176em
}}

.c-product__sep {{
    top: 1px
}}

.c-product__sep+.c-product__sep {{
    display: none
}}

.c-product__price {{
    font-weight: 600
}}

.c-product__rating {{
    display: inline-block;
    position: relative;
    margin-bottom: 0;
    line-height: 1.67;
    font-size: 0.9176em;
    color: #888;
    top: -1px
}}

.c-product__rating .stars {{
    font-size: 1.33em;
    top: -1px
}}

.c-product__title a,
.c-product__subtitle a,
.c-product__price a,
.c-product__rating a {{
    color: inherit
}}

.c-product__callout {{
    margin-top: 0.5em;
    display: block
}}

@media only screen and (max-width: 590px) {{
    .c-product__media {{
        line-height: initial;
        overflow: hidden;
        border-right: none;
        margin-right: 0;
        padding-right: 0;
        max-width: 120px;
        max-height: 120px;
        height: auto;
        float: right
    }}
    .c-product__media__img {{
        vertical-align: top;
        max-width: 100%;
        max-width: 120px;
        max-height: 120px
    }}
    .c-product__body {{
        line-height: normal;
        padding-right: 0;
        padding-left: 0;
        border-left: none;
        border-top: none
    }}
    .c-product__body-wrap {{
        overflow: visible
    }}
    .c-product__body-content {{
        display: block
    }}
    .c-product__title {{
        font-size: 1.1em
    }}
    .c-product__subtitle {{
        display: block;
        position: absolute;
        max-height: 4.5em;
        max-width: 100%;
        bottom: 0;
        left: 0
    }}
    .c-product__subtitle .sep {{
        display: none
    }}
    .c-product__sep {{
        display: none
    }}
    .c-product__price,
    .c-product__rating {{
        display: block
    }}
    .c-product__brand {{
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis;
        max-height: 1.2em;
        max-width: 100%;
        display: block
    }}
    .c-product__callout {{
        margin-left: 0;
        margin-right: 0
    }}
    .c-product__callout .btn {{
        display: block
    }}
    .c-product__body--pri {{
        padding-bottom: 4em;
        min-height: 120px
    }}
    .c-product__body--sec {{
        clear: both;
        overflow: visible;
        margin-top: 0.5em;
        margin-right: 0;
        padding-top: 0.5em;
        border-top: 1px solid #dbdbdb
    }}
}}

.c-detail {{
    display: inline-block;
    vertical-align: middle;
    line-height: 1.2;
    max-width: 100%
}}

.c-detail__title a,
.c-detail__desc a,
.c-detail__more a,
.c-detail__rating a {{
    color: inherit
}}

.c-detail__user,
.c-detail__count,
.c-detail__date {{
    color: #333;
    font-size: 0.85em
}}

.c-detail__user .c-detail__icon,
.c-detail__count .c-detail__icon,
.c-detail__date .c-detail__icon {{
    font-size: 1.25em;
    padding-right: 0.25em
}}

.c-detail__more {{
    margin-top: 2em;
    font-size: 0.85em
}}

.c-detail__more:link:hover {{
    text-decoration: none;
    color: #4495d4
}}

.c-detail__more .c-detail__icon {{
    margin-right: 0.5em;
    margin-top: -0.25em;
    width: 16px;
    max-height: 16px
}}

.is-mobile .c-detail__more {{
    margin-top: 1em
}}

.c-detail__callout {{
    display: block;
    margin-top: 1em
}}

.c-detail__links {{
    width: 115%;
    font-size: 0.9em;
    margin-top: 1.5em
}}

.c-detail__links+.c-detail__more {{
    margin-top: 1.5em
}}

.c-detail__links--title {{
    font-size: 0.85em;
    margin-top: 0.5em
}}

.c-detail__links--title+.c-detail__links {{
    margin-top: 0
}}

.c-detail__links__btn,
.c-detail__callout .btn {{
    margin-right: 0.5em;
    margin-bottom: 0.5em;
    position: relative;
    line-height: 2.0
}}

.c-detail__title {{
    max-width: 25em;
    padding-top: 0;
    color: #333
}}

.c-detail__subtitle {{
    margin-top: -0.5em
}}

.c-detail__title__sub,
.c-detail__desc,
.c-detail__filemeta,
.c-detail__more {{
    font-size: 0.9em;
    color: #888
}}

.c-detail__title__sub {{
    font-size: 0.8em;
    display: block
}}

.c-detail__price {{
    font-weight: 600
}}

.c-detail__rating {{
    margin-bottom: 0.5em;
    line-height: 1.67;
    font-size: 0.8em;
    color: #888
}}

.c-detail__rating .stars {{
    font-size: 1.45em;
    top: -1px
}}

.c-detail__sep {{
    top: 0.05em
}}

.c-detail__sep+.c-detail__sep {{
    display: none
}}

.c-detail__icon {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale
}}

.c-detail__icon {{
    color: #aaa;
    display: inline-block;
    vertical-align: middle;
    margin-top: -2px
}}

.c-detail__source .stars {{
    top: -0.125em
}}

@media only screen and (max-width: 590px) {{
    .c-detail__title {{
        font-size: 1.1em
    }}
}}

.zcm-wrap {{
    line-height: 38px;
    max-height: 38px;
    overflow: hidden;
    overflow-x: auto;
    overflow-style: move;
    white-space: nowrap;
    -webkit-overflow-scrolling: touch;
    position: relative
}}

.zcm-wrap::-webkit-scrollbar {{
    display: none
}}

.lt-ie8 .zcm-wrap {{
    margin-bottom: -0.625em
}}

.zcm-wrap.is-hidden {{
    max-height: 0;
    margin-top: 0
}}

.no-touch .zcm-wrap {{
    overflow: hidden
}}

.zcm-wrap--header {{
    margin-left: 7px
}}

.zcm-wrap--tiles {{
    padding-right: 6em;
    overflow: hidden
}}

@media only screen and (max-width: 590px) {{
    .zcm-wrap--tiles {{
        padding-right: 0;
        margin-right: 0
    }}
}}

.zcm-wrap--tiles .zcm__menu--tiles {{
    z-index: 1;
    position: absolute;
    background-color: #f2f2f2
}}

.zcm {{
    padding-left: 3px;
    white-space: nowrap;
    height: 38px
}}

.zcm--sub {{
    font-size: 0.9em;
    white-space: nowrap;
    height: 33px
}}

.zcm--topics {{
    overflow: hidden;
    margin-right: 6em
}}

.zcm--topics .zcm__item:last-child {{
    margin-right: 2em
}}

.is-mobile-device .zcm--topics {{
    -webkit-overflow-scrolling: touch;
    overflow-x: auto
}}

.zcm__menu {{
    float: left;
    margin: 0;
    padding: 0 0 1px;
    text-transform: capitalize
}}

.zcm__menu--tiles {{
    position: relative;
    float: right;
    right: 0;
    top: 0.125em
}}

.zcm__drop {{
    position: relative;
    padding: 0
}}

.zcm__drop .zcm__sep--h {{
    display: none
}}

.zcm__drop .zcm__menu {{
    background: #fff;
    background: rgba(255, 255, 255, 0.95);
    padding-top: 0.25em;
    padding-bottom: 0.25em;
    float: none;
    display: none;
    position: absolute;
    top: 2em;
    right: 0
}}

.zcm__drop .zcm__item {{
    padding-bottom: 0;
    margin-bottom: 0;
    margin-top: 0;
    display: block
}}

.zcm__drop.is-open .zcm__menu {{
    display: block
}}

.zcm__drop--main {{
    position: absolute;
    text-align: right;
    bottom: 0;
    right: 0;
    z-index: 1
}}

.zcm__drop--main .zcm__menu {{
    top: 2.8em
}}

.zcm__drop--main .zcm__link {{
    padding-left: 2em;
    padding-right: 0.8em
}}

.zcm__dynamic {{
    display: none
}}

.zcm__dynamic.has-zci {{
    display: inline-block
}}

.is-mobile .zcm__menu:last-child .zcm__item:last-child {{
    padding-right: 30px
}}

.zcm__constant,
.zcm__dynamic.has-zci {{
    display: inline-block;
    float: none;
    overflow: hidden;
    white-space: nowrap
}}

.zcm__constant:after,
.zcm__dynamic.has-zci:after {{
    display: none
}}

.zcm__sep--h:before,
.zcm__sep {{
    vertical-align: top;
    margin: 0 7px;
    height: 1em
}}

.zcm__item,
.zcm__item--b,
.zcm__sep--h {{
    list-style: none;
    overflow: hidden;
    vertical-align: top;
    display: inline-block
}}

.zcm__item--b {{
    overflow: visible
}}

.zcm__link {{
    display: block;
    font-size: 1em;
    position: relative;
    margin: 0 7px;
    height: 37px;
    line-height: 37px;
    font-weight: 600;
    -moz-osx-font-smoothing: grayscale;
    -webkit-font-smoothing: antialiased;
    -webkit-tap-highlight-color: transparent
}}

.zcm__link,
.zcm__link:focus,
.zcm__link:hover {{
    text-decoration: none;
    color: #666
}}

.zcm__link.is-active {{
    color: #de5833;
    border-bottom: solid 1px #de5833
}}

.at-zci-bottom .zcm__link.is-active {{
    color: #666;
    border-bottom-width: 0
}}

.zcm__link .zcm__underline {{
    position: absolute;
    display: block;
    bottom: -1px;
    left: 7px;
    right: 7px;
    height: 1px;
    background: #de5833;
    opacity: 0
}}

@media only screen and (max-width: 590px) {{
    .zcm__link .zcm__underline {{
        bottom: 0
    }}
}}

.zcm__link.is-highlighted {{
    color: #de5833
}}

.zcm__link.is-highlighted .zcm__underline {{
    opacity: 1;
    -webkit-animation: blink-opacity 0.75s linear 4;
    -moz-animation: blink-opacity 0.75s linear 4;
    -ms-animation: blink-opacity 0.75s linear 4;
    -o-animation: blink-opacity 0.75s linear 4;
    animation: blink-opacity 0.75s linear 4
}}

.at-zci-bottom.has-active-zci .zcm__link:before,
.at-zci-bottom.has-active-zci .zcm__link:after {{
    visibility: hidden
}}

.no-touch .zcm__link:hover {{
    color: #333
}}

.no-touch .zcm__link:hover:active,
.no-touch .zcm__link:focus:active {{
    color: #de5833
}}

.no-touch .zcm__link.is-active:hover,
.no-touch .zcm__link.is-active:hover:active,
.no-touch .zcm__link.is-active:focus:active {{
    color: #de5833
}}

.zcm__item__underline {{
    height: 1px;
    background: #de5833;
    position: absolute;
    z-index: 20
}}

.zcm__sep--h {{
    padding: 0;
    height: 37px;
    line-height: 37px;
    margin-left: -2px;
    margin-right: -1em
}}

.zcm__sep--h+.zcm__item {{
    padding-left: 1em
}}

.zcm__sep--h.is-hidden+.zcm__item {{
    padding-left: 0
}}

.lt-ie9 .zcm__sep--h {{
    margin-right: 0
}}

.lt-ie9 .zcm__sep--h+.zcm__item {{
    padding-left: 0
}}

.zcm__sep--h:before {{
    vertical-align: baseline;
    top: 0.1em
}}

.zcm__link--sub {{
    display: block;
    cursor: pointer;
    position: relative;
    padding: 0 7px
}}

.zcm__link--sub,
.zcm__link--sub:visited {{
    color: #999
}}

.zcm__link--sub:hover,
.zcm__link--sub.is-highlighted {{
    color: black
}}

.zcm__link--sub.is-selected,
.zcm__item.is-selected .zcm__link--sub {{
    color: #de5833
}}

.zcm__link__count {{
    font-size: 0.6em;
    position: relative;
    padding: 0.5em 0 0 0.5em;
    margin: -0.25em 0;
    top: -0.8em
}}

.zcm__link--sub--alpha {{
    left: 3.5px;
    padding-left: 3.5px;
    padding-right: 3.5px
}}

.zcm__link--sub--alpha .zcm__link__count {{
    display: none
}}

@media only screen and (max-width: 590px) {{
    .zcm-wrap {{
        max-width: 102%
    }}
    .zcm--sub,
    .zcm--topics {{
        margin-left: -0.5em
    }}
    .zcm--topics {{
        margin-right: 4.5em
    }}
}}

@-webkit-keyframes blink-opacity {{
    0% {{
        opacity: 0
    }}
    49% {{
        opacity: 0
    }}
    50% {{
        opacity: 1
    }}
    100% {{
        opacity: 1
    }}
}}

@-moz-keyframes blink-opacity {{
    0% {{
        opacity: 0
    }}
    49% {{
        opacity: 0
    }}
    50% {{
        opacity: 1
    }}
    100% {{
        opacity: 1
    }}
}}

@-o-keyframes blink-opacity {{
    0% {{
        opacity: 0
    }}
    49% {{
        opacity: 0
    }}
    50% {{
        opacity: 1
    }}
    100% {{
        opacity: 1
    }}
}}

@keyframes blink-opacity {{
    0% {{
        opacity: 0
    }}
    49% {{
        opacity: 0
    }}
    50% {{
        opacity: 1
    }}
    100% {{
        opacity: 1
    }}
}}

.zci-wrap {{
    background-color: #f2f2f2;
    clear: both;
    display: block;
    position: relative;
    z-index: 10;
    margin-top: -1px
}}

.is-mobile.has-open-detail .zci-wrap {{
    z-index: 101
}}

.zci-wrap:after {{
    content: "";
    clear: both;
    display: table
}}

.zci {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    clear: both;
    position: relative;
    width: 100%;
    color: #666;
    background-color: #f2f2f2;
    padding: 0 7px;
    line-height: 1.37;
    visibility: hidden;
    overflow: hidden;
    max-height: 0;
    display: none;
    z-index: 1
}}

.zci.is-active {{
    border-bottom: 1px solid #dbdbdb;
    padding-bottom: 1em;
    padding-top: 1em;
    margin-top: 1px;
    visibility: visible;
    overflow: visible;
    max-height: none;
    display: block
}}

.zci.is-active .zci.is-active {{
    margin-top: 0
}}

.zci h1,
.zci h2,
.zci h3 {{
    padding-top: 0;
    padding-bottom: 0
}}

.zci h1 a,
.zci h2 a,
.zci h3 a {{
    color: inherit
}}

.zci .cw {{
    padding: 0.25em 0
}}

.zci pre {{
    border: 1px solid #dbdbdb;
    padding: 5px;
}}

.zci .zci {{
    border-top: 1px solid #dbdbdb;
    border-bottom: none;
    padding-bottom: 1em;
    padding-top: 1em
}}

.zci .zci:before {{
    display: none
}}

.zci--type--tiles {{
    padding-left: 0;
    padding-right: 0
}}

.zci--type--tiles.is-active {{
    padding-bottom: 0;
    padding-top: 0
}}

.zci--no-max {{
    max-height: none
}}

.zci-link {{
    color: #4495d4
}}

.zci--placeholder {{
    height: 10em
}}

.zci--placeholder .zci__loading-error {{
    color: #888
}}

.zci--placeholder .zci__loading-error:before {{
    content: "\2639";
    display: inline-block;
    vertical-align: middle;
    color: #c2c2c2;
    font-size: 3em;
    padding-right: .25em
}}

.zci--no-spacing {{
    padding: 0;
    margin: 0
}}

.zci__main {{
    -moz-transition: padding-left 0.3s ease-in-out 0s;
    -o-transition: padding-left 0.3s ease-in-out 0s;
    -webkit-transition: padding-left 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: padding-left 0.3s ease-in-out 0s;
    clear: both
}}

.is-mobile-device .zci__main {{
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s
}}

.zci__aux {{
    display: none;
    position: absolute;
    top: 0.5em;
    bottom: 0.5em;
    right: 0.6em;
    width: 20%;
    min-width: 11em
}}

@media only screen and (min-width: 980px) {{
    .zci__aux {{
        display: block
    }}
}}

@media only screen and (min-width: 971.1px) {{
    .zci__aux {{
        width: 27%
    }}
}}

.cw.has-aux {{
    width: auto
}}

@media only screen and (min-width: 864px) {{
    .cw.has-aux {{
        width: 77%
    }}
}}

@media only screen and (min-width: 971.1px) {{
    .cw.has-aux {{
        width: 67%
    }}
}}

.zci__caption {{
    margin: 0;
    display: block;
    overflow: hidden;
    font-size: 1.5em;
    font-weight: 400;
    line-height: 1.2;
    color: #333
}}

.zci__header {{
    margin-top: 0;
    margin-bottom: 0;
    padding-bottom: 0;
    padding-top: 0;
    display: block;
    overflow: hidden;
    font-size: 1.31em;
    font-weight: 600;
    line-height: 1.2;
    color: #333
}}

.zci__subheader,
.zci__header__sub {{
    font-size: 14px;
    font-size: 0.97222rem;
    margin-bottom: 0;
    margin-top: 0;
    padding-bottom: 0;
    padding-top: 0.25em;
    display: block;
    font-weight: normal;
    line-height: 1.2;
    color: #888
}}

.zci__header__sub {{
    padding-left: 0.5em;
    display: inline-block;
    text-transform: capitalize
}}

@media only screen and (min-width: 590px) and (min-height: 738px) {{
    .zci__header__sub {{
        padding-left: 0;
        display: block
    }}
}}

@media only screen and (max-width: 590px) {{
    .zci__header__sub {{
        padding-left: 0;
        display: block
    }}
}}

.zci__header--detail {{
    font-weight: 400
}}

.zci__header--detail b {{
    font-weight: 600
}}

.zci__body-wrap {{
    display: block;
    overflow: hidden
}}

.zci__body {{
    padding-left: 10px
}}

.zci__content {{
    margin-top: 0.75em
}}

.zci__body--no-max {{
    padding-left: 10px
}}

.zci__rule {{
    display: block;
    width: 100%;
    border: none;
    border-bottom: 1px solid #dbdbdb;
    margin: 0 auto 0.75em;
    padding: 0;
    height: 0
}}

.zci__result,
.zci__input {{
    word-wrap: break-word
}}

.zci__links,
.zci__more-at-wrap {{
    margin-top: 0.75em;
    display: block;
    clear: both
}}

.zci__more-at {{
    display: inline-block;
    font-size: 0.9176em
}}

.zci__more-at,
.zci__more-at:focus {{
    color: #888
}}

.zci__more-at:hover {{
    color: #4495d4
}}

.zci__more-at__icon {{
    vertical-align: middle;
    padding-right: 0.5em;
    # margin-top: -3px;
    max-width: 16px
}}

.is-mobile .zci-wrap {{
    z-index: 13
}}

.is-mobile .zci__main {{
    padding-left: 0
}}

.is-mobile .zci__body {{
    display: block;
    margin-left: -1px;
    padding: 0 1.25em
}}

.is-mobile .has-aux.cw {{
    position: static;
    padding-bottom: 0
}}

.is-mobile .has-aux.cw .zci__main {{
    position: static
}}

.is-mobile .has-aux+.zci__aux {{
    display: block;
    overflow: hidden;
    min-width: initial;
    margin-bottom: 0;
    height: auto;
    width: auto;
    top: auto;
    left: 0;
    right: 0;
    bottom: 0;
    position: relative;
    padding-top: 1em;
    max-height: 7.85em
}}

.is-mobile .is-expanded .has-aux+.zci__aux {{
    max-height: none
}}

.is-mobile .is-expanded .has-aux+.zci__aux .tile--info .info__value {{
    height: auto;
    white-space: normal;
    display: inline
}}

.is-mobile .has-aux+.zci__aux .tile--info {{
    -moz-box-shadow: none;
    -webkit-box-shadow: none;
    box-shadow: none;
    background: none;
    border: none
}}

.is-mobile .has-aux+.zci__aux .tile--info .tile__expand {{
    display: none
}}

.is-mobile .tile--info {{
    font-size: 1em;
    position: static;
    padding: 0 1.25em
}}

.is-mobile .tile--info .info {{
    border-top: none;
    margin-top: 1px;
    padding-bottom: 0;
    font-size: 0.9em
}}

.is-mobile .tile--info .info__label {{
    text-transform: uppercase;
    font-size: 0.9em;
    padding-right: 0.25em;
    margin-bottom: 0;
    margin-top: 1px;
    float: left
}}

.is-mobile .tile--info .info__label:after {{
    content: " \2014 "
}}

.is-mobile .tile--info .info__value {{
    white-space: nowrap;
    overflow: hidden;
    -ms-text-overflow: ellipsis;
    -o-text-overflow: ellipsis;
    text-overflow: ellipsis;
    height: 1.4em;
    margin-top: 0;
    margin-bottom: 0
}}

.zci__no-results {{
    color: #aaa;
    position: absolute;
    text-align: center;
    display: block;
    margin: auto;
    bottom: 0;
    top: 0;
    left: 0;
    right: 0;
    width: 100%;
    height: 8.75em
}}

.zci__no-results:before,
.zci__no-results:after {{
    display: block
}}

.zci__no-results:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    content: "\2639";
    font-size: 7em;
    color: #c2c2c2;
    line-height: 1;
    width: 1em;
    margin: 0 auto
}}

.zci__no-results__txt {{
    font-size: 1.25em;
    font-weight: 600;
    display: block
}}

@media only screen and (max-width: 590px) {{
    .zci {{
        padding-left: 0;
        padding-right: 0
    }}
}}

.tile-wrap {{
    position: relative;
    top: 0;
    margin-top: 0;
    margin-bottom: 0
}}

.tile-wrap .tile {{
    cursor: pointer
}}

.is-mobile .metabar--sticky+.tile-wrap,
.is-mobile .metabar--unsticky+.tile-wrap {{
    padding-top: 1px
}}

.is-mobile .metabar--sticky.is-hidden+.tile-wrap,
.is-mobile .metabar--unsticky.is-hidden+.tile-wrap {{
    padding-top: 0.5em
}}

.has-tiles {{
    -webkit-overflow-scrolling: touch;
    overflow-y: hidden;
    overflow-x: auto;
    overflow-style: move;
    white-space: nowrap;
    position: static
}}

.has-tiles .tile,
.has-tiles .tile--s {{
    display: inline-block;
    white-space: normal;
    vertical-align: top;
    float: none;
    left: 12px;
    margin-top: 4px;
    margin-bottom: 0.75em
}}

.has-tiles .tile:last-child,
.has-tiles .tile--s:last-child {{
    margin-right: 100px
}}

.no-touch .has-tiles {{
    overflow-x: hidden
}}

.at-topic .tile {{
    background-color: #f7f7f7
}}

.at-topic .tile.highlight {{
    background-color: #fff
}}

.at-topic .tile.highlight .tile__title {{
    color: #333
}}

.at-topic .tile__title {{
    color: #888
}}

.at-topic .tile__check {{
    visibility: hidden;
    opacity: 0
}}

.at-topic .tile.active-topic {{
    background-color: #fff
}}

.at-topic .tile.active-topic .tile__title {{
    color: #333
}}

.at-topic .tile.active-topic .tile__check {{
    visibility: visible;
    opacity: 1
}}

.tileview {{
    min-height: 150px;
    min-height: 10.41667rem
}}

.tileview .metabar--fixed {{
    position: relative;
    top: 0 !important;
    background-color: #f2f2f2
}}

.tileview .metabar__mode {{
    text-indent: 3px;
    line-height: 36px
}}

.tileview .detail {{
    top: 300px
}}

.tileview--grid .zci__main,
.tileview--grid .zci__menu,
.tileview--grid .zci__detail,
.tileview--grid .metabar__in {{
    padding-left: 0
}}

.tileview--grid .metabar--sticky,
.tileview--grid .metabar--unsticky {{
    padding-bottom: 4px
}}

.tileview--grid .metabar--fixed+.tile-wrap {{
    margin-top: 3px !important;
    padding-top: 0
}}

.set-header--fixed .tileview--grid .metabar--fixed,
.tileview--grid .metabar--fixed.is-stuck {{
    padding-bottom: 0;
    padding-left: 7px;
    padding-right: 7px;
    background-color: #f2f2f2;
    background-color: rgba(242, 242, 242, 0.96);
    border-top: 1px solid #f2f2f2;
    border-top-color: rgba(242, 242, 242, 0.96);
    position: fixed;
    width: 100%
}}

.set-header--fixed .tileview--grid .metabar--fixed+.tile-wrap,
.tileview--grid .metabar--fixed.is-stuck+.tile-wrap {{
    padding-top: 39px
}}

.set-header--fixed .tileview--grid .metabar--fixed.is-hidden+.tile-wrap,
.tileview--grid .metabar--fixed.is-stuck.is-hidden+.tile-wrap {{
    margin-bottom: 0;
    padding-top: 0
}}

.tileview--grid .metabar--fixed.is-stuck {{
    -moz-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    -webkit-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    border-bottom: 1px solid #dbdbdb;
    border-bottom-color: transparent;
    padding-bottom: 4px
}}

.tileview--grid .metabar--fixed.is-stuck.at-bottom {{
    top: auto !important;
    position: absolute;
    bottom: 0px
}}

.tileview--grid .detail {{
    bottom: auto
}}

.tileview--grid .tile {{
    list-style: none;
    display: inline-block;
    vertical-align: top;
    margin-top: 0
}}

.tileview--grid .tile--c--whole {{
    width: auto;
    display: block;
    margin-left: 0.275em;
    margin-right: 0.275em
}}

.tileview--grid .tile--c,
.tileview--grid .tile--m {{
    width: 24%;
    margin-left: 0.5%;
    margin-right: 0.5%
}}

@media only screen and (min-width: 971.1px) {{
    .tileview--grid .tile--c,
    .tileview--grid .tile--m {{
        width: 19%
    }}
}}

@media only screen and (min-width: 1186.9px) {{
    .tileview--grid .tile--c,
    .tileview--grid .tile--m {{
        margin-left: 0.25%;
        margin-right: 0.25%;
        width: 16.15%
    }}
}}

@media only screen and (min-width: 1440px) {{
    .tileview--grid .tile--c,
    .tileview--grid .tile--m {{
        width: 13.78%
    }}
}}

@media only screen and (min-width: 1584px) {{
    .tileview--grid .tile--c,
    .tileview--grid .tile--m {{
        width: 12%
    }}
}}

@media only screen and (min-width: 1800px) {{
    .tileview--grid .tile--c,
    .tileview--grid .tile--m {{
        width: 10.61%
    }}
}}

@media only screen and (min-width: 1944px) {{
    .tileview--grid .tile--c,
    .tileview--grid .tile--m {{
        margin-left: 0.125%;
        margin-right: 0.125%;
        width: 8.825%
    }}
}}

@media only screen and (max-width: 590px) {{
    .tileview--grid .tile--c,
    .tileview--grid .tile--m {{
        width: 32.3%
    }}
}}

@media only screen and (max-width: 425px) {{
    .tileview--grid .tile--c,
    .tileview--grid .tile--m {{
        width: 48%;
        margin-left: 1%;
        margin-right: 1%
    }}
}}

.tileview--grid .tile--c--w {{
    width: 24%;
    margin-left: 0.5%;
    margin-right: 0.5%
}}

@media only screen and (min-width: 1186.9px) {{
    .tileview--grid .tile--c--w {{
        width: 19%
    }}
}}

@media only screen and (min-width: 1440px) {{
    .tileview--grid .tile--c--w {{
        margin-left: 0.25%;
        margin-right: 0.25%;
        width: 16.15%
    }}
}}

@media only screen and (min-width: 1584px) {{
    .tileview--grid .tile--c--w {{
        width: 13.78%
    }}
}}

@media only screen and (min-width: 1800px) {{
    .tileview--grid .tile--c--w {{
        width: 12%
    }}
}}

@media only screen and (min-width: 1944px) {{
    .tileview--grid .tile--c--w {{
        width: 10.61%
    }}
}}

@media only screen and (max-width: 864px) {{
    .tileview--grid .tile--c--w {{
        width: 32.3%
    }}
}}

@media only screen and (max-width: 590px) {{
    .tileview--grid .tile--c--w {{
        width: 48%;
        margin-left: 1%;
        margin-right: 1%
    }}
}}

.tileview--grid .tile--c--n {{
    width: 15.65%;
    margin-left: 0.5%;
    margin-right: 0.5%
}}

@media only screen and (min-width: 1079px) {{
    .tileview--grid .tile--c--n {{
        margin-left: 0.25%;
        margin-right: 0.25%;
        width: 13.75%
    }}
}}

@media only screen and (min-width: 1186.9px) {{
    .tileview--grid .tile--c--n {{
        width: 11.98%
    }}
}}

@media only screen and (min-width: 1440px) {{
    .tileview--grid .tile--c--n {{
        width: 10.59%
    }}
}}

@media only screen and (min-width: 1584px) {{
    .tileview--grid .tile--c--n {{
        width: 9.49%
    }}
}}

@media only screen and (min-width: 1800px) {{
    .tileview--grid .tile--c--n {{
        margin-left: 0.125%;
        margin-right: 0.125%;
        width: 8.825%
    }}
}}

@media only screen and (min-width: 1944px) {{
    .tileview--grid .tile--c--n {{
        width: 8.05%
    }}
}}

@media only screen and (max-width: 864px) {{
    .tileview--grid .tile--c--n {{
        width: 19%
    }}
}}

@media only screen and (max-width: 727px) {{
    .tileview--grid .tile--c--n {{
        width: 24%
    }}
}}

@media only screen and (max-width: 501.5px) {{
    .tileview--grid .tile--c--n {{
        width: 32.3%
    }}
}}

@media only screen and (max-width: 425px) {{
    .tileview--grid .tile--c--n {{
        width: 48%;
        margin-left: 1%;
        margin-right: 1%
    }}
}}

.is-mobile .tileview--grid .metabar__primary-text {{
    padding-left: 0
}}

@media only screen and (max-width: 590px) {{
    .is-mobile .tileview--grid .tile--c--w--mob,
    .is-mobile .tileview--grid .tile--news,
    .is-mobile .tileview--grid .tile--qa {{
        margin-left: 1%;
        margin-right: 1%;
        width: 48%
    }}
}}

@media only screen and (max-width: 425px) {{
    .is-mobile .tileview--grid .tile--c--w--mob,
    .is-mobile .tileview--grid .tile--news,
    .is-mobile .tileview--grid .tile--qa {{
        width: auto;
        display: block;
        margin-left: 0.275em;
        margin-right: 0.275em
    }}
}}

.is-mobile .tileview--grid .metabar--sticky,
.is-mobile .tileview--grid .metabar--unsticky {{
    padding: 2px 0.25em 0 0.7em;
    margin-bottom: 0
}}

.is-mobile .has-tiles--grid {{
    min-height: initial;
    overflow: hidden
}}

.is-mobile .has-tiles--grid .tile {{
    margin-bottom: 7.2px;
    margin-bottom: 0.5rem;
    margin-top: 1px
}}

.is-mobile .has-tiles--grid .tile--m {{
    -webkit-appearance: none;
    -moz-appearance: none;
    -ms-appearance: none;
    -o-appearance: none;
    appearance: none;
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s;
    -moz-border-radius: 0.25em;
    -webkit-border-radius: 0.25em;
    border-radius: 0.25em;
    outline: none !important;
    text-decoration: none !important;
    background-color: #fafafa;
    border: 1px solid #ddd;
    cursor: pointer;
    -webkit-user-select: none;
    -khtml-user-select: none;
    -moz-user-select: -moz-none;
    -ms-user-select: none;
    user-select: none;
    width: auto;
    height: auto;
    display: block;
    font-weight: 600;
    padding: 0;
    margin: 0 0.25em 0.25em;
    font-size: 1em;
    line-height: 3
}}

.is-mobile .has-tiles--grid .tile--m,
.is-mobile .has-tiles--grid .tile--m:hover {{
    color: #333
}}

.has-tiles .anchor--inline,
.has-tiles--grid .anchor--inline {{
    margin-left: 0
}}

.tileview__other {{
    min-height: 210px
}}

.tile--page-break {{
    width: 100%
}}

.tile--page-break:after {{
    clear: both;
    content: "";
    display: table
}}

.tile__body--b-i {{
    padding: .75em 1em
}}

.is-mobile .tile__body--b-i {{
    padding: .75em
}}

.tile--b--i--vid .tile__media,
.tile--b--i--mov .tile__media,
.tile--vid .tile__media {{
    height: 0
}}

.tile--b--i--vid .tile__media__img,
.tile--b--i--mov .tile__media__img,
.tile--vid .tile__media__img {{
    text-indent: -9999px;
    background-color: #f7f7f7;
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0
}}

.tile--b--i--vid {{
    width: 18.35em
}}

.tile--b--i--vid .tile__media {{
    padding-bottom: 56.25%
}}

.tile--b--i--mov {{
    width: 12em
}}

.tile--b--i--mov .tile__media {{
    padding-bottom: 148.333%
}}

.tile--b--i--mov .tile__media__img {{
    min-width: 100%;
    height: 100%
}}

.tile--info {{
    padding-bottom: 2.5em;
    padding-top: 0.5em;
    position: absolute;
    margin: auto;
    bottom: 0;
    top: 0;
    overflow: hidden;
    max-height: 100%;
    width: 100%
}}

.tile--info.is-open {{
    -moz-box-shadow: 0 1px 4px rgba(0, 0, 0, 0.1);
    -webkit-box-shadow: 0 1px 4px rgba(0, 0, 0, 0.1);
    box-shadow: 0 1px 4px rgba(0, 0, 0, 0.1);
    max-height: 1000%;
    min-height: 100%;
    bottom: auto
}}

.tile--info.is-open .tile__foot hr {{
    border-bottom: none
}}

.tile--info .info {{
    border-top: 1px solid #f2f2f2
}}

.tile--info .info:first-child {{
    border-top-color: transparent
}}

.tile--info .info.one-line {{
    padding-right: 1em
}}

.tile--info .info--head {{
    border-top: none;
    border-bottom: 1px solid #f2f2f2;
    margin-bottom: -1px
}}

.tile--info__link {{
    color: #666
}}

.tile--info__link:hover {{
    color: #4495d4
}}

.tile--info__link:hover:before {{
    display: inline-block
}}

.tile--info__link:before {{
    content: "\bb";
    display: none;
    position: absolute;
    line-height: 2;
    right: 0;
    top: 0
}}

.tile--info__link .info__label {{
    color: inherit
}}

.mapview {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    position: relative;
    width: 100%;
    padding: 0
}}

.is-mobile .mapview {{
    overflow: hidden
}}

.mapview,
.mapview.is-active {{
    padding-top: 0
}}

.mapview.is-active {{
    min-height: 242px;
    padding-bottom: 0;
    border-bottom: none
}}

.mapview.is-active.has-tileview {{
    border-bottom: 1px solid #dbdbdb;
    border-bottom-color: rgba(0, 0, 0, 0.15)
}}

.is-mobile .mapview.is-active {{
    margin-top: 0
}}

.mapview:before {{
    display: none
}}

.mapview.has-tileview {{
    -moz-transition: height 0.3s ease-in-out 0s;
    -o-transition: height 0.3s ease-in-out 0s;
    -webkit-transition: height 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: height 0.3s ease-in-out 0s;
    min-height: 0
}}

.is-mobile-device .mapview.has-tileview {{
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s
}}

.mapview.is-expanded {{
    overflow: hidden
}}

.mapview.is-expanded .zci {{
    -moz-box-shadow: 0 -1px 0 rgba(0, 0, 0, 0.15);
    -webkit-box-shadow: 0 -1px 0 rgba(0, 0, 0, 0.15);
    box-shadow: 0 -1px 0 rgba(0, 0, 0, 0.15);
    border-top-color: transparent;
    position: absolute;
    background-color: #e6e6e6;
    background-color: rgba(230, 230, 230, 0.75)
}}

.mapview.is-expanded .zci .has-tiles {{
    padding-top: 0.5em
}}

.mapview.is-expanded .metabar {{
    -moz-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    -webkit-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    border-bottom: 1px solid #dbdbdb;
    border-bottom-color: transparent;
    background-color: #e6e6e6;
    background-color: rgba(230, 230, 230, 0.9);
    position: relative;
    top: 0 !important;
    z-index: 2
}}

.mapview.is-expanded .metabar__in {{
    padding-bottom: 3px
}}

.mapview .metabar {{
    padding-top: 0
}}

.mapview .metabar__in {{
    padding-top: 5px;
    padding-bottom: 0;
    position: static
}}

.mapview .metabar__mode {{
    background-color: rgba(0, 0, 0, 0.1)
}}

.mapview .metabar__mode:hover {{
    background-color: #666
}}

.is-mobile .mapview .mapview__close {{
    color: #fff;
    background-color: #a0a0a0;
    background-color: rgba(160, 160, 160, 0.85)
}}

.mapview .zci {{
    border-color: transparent;
    padding-top: 0;
    padding-bottom: 0;
    bottom: 0;
    z-index: 1;
    width: 100%
}}

.is-mobile .mapview .zci {{
    margin-top: 0;
    padding-top: 2px
}}

.mapview .zci .tile-wrap {{
    margin: 0
}}

.mapview .zci .has-tiles {{
    padding-top: 0;
    padding-bottom: 0
}}

.mapview .tileview {{
    margin: 0;
    position: relative
}}

.tile--m--maps_places {{
    width: 9.25em;
    height: 15em;
    margin: 0 0.25em
}}

.is-expanded .tile--m--maps_places {{
    background: rgba(210, 210, 210, 0.75)
}}

.is-mobile .tile--m--maps_places .hide--mob {{
    display: block
}}

.is-mobile .tile--m--maps_places .tile--m--mob {{
    display: none
}}

.mapview__map {{
    position: absolute;
    top: 0;
    left: 0;
    width: 100%;
    height: 100%
}}

.zci+.mapview__map {{
    z-index: -1
}}

.is-expanded .zci+.mapview__map {{
    z-index: 0
}}

.mapview__map__bottom-shadow {{
    -moz-box-shadow: 0 -1px 0 rgba(0, 0, 0, 0.15);
    -webkit-box-shadow: 0 -1px 0 rgba(0, 0, 0, 0.15);
    box-shadow: 0 -1px 0 rgba(0, 0, 0, 0.15);
    position: absolute;
    bottom: -1px;
    left: 0;
    width: 100%;
    height: 1px;
    z-index: 20
}}

.lt-ie9 .mapview__map__bottom-shadow {{
    border-top: 1px solid #dbdbdb;
    bottom: 0
}}

.mapview__close {{
    position: absolute;
    top: 8px;
    right: 2px
}}

.mapview-marker {{
    background-image: url("/js/mapbox/ddgimages/map-pin_blank.v101.png");
    background-size: 26px 46px;
    text-align: center;
    cursor: pointer;
    outline: none
}}

.mapview-marker:hover,
.mapview-marker.has-focus {{
    background-image: url("/js/mapbox/ddgimages/map-pin_active.png")
}}

.mapview-marker.mapview-marker-star,
.mapview-marker.mapview-marker-star.has-focus {{
    background-image: url("/js/mapbox/ddgimages/map-pin_star.v101.png")
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    .mapview-marker {{
        background-image: url("/js/mapbox/ddgimages/map-pin_blank@2x.v101.png")
    }}
    .mapview-marker:hover,
    .mapview-marker.has-focus {{
        background-image: url("/js/mapbox/ddgimages/map-pin_active@2x.png")
    }}
    .mapview-marker.mapview-marker-star,
    .mapview-marker.mapview-marker-star.has-focus {{
        background-image: url("/js/mapbox/ddgimages/map-pin_star@2x.v101.png")
    }}
}}

.mapview-marker__icn,
.mapview-marker__num {{
    color: #fff;
    line-height: 26px;
    display: block
}}

.mapview-marker__num {{
    font-family: "DDG_ProximaNova", "DDG_ProximaNova_UI_0", "DDG_ProximaNova_UI_1", "DDG_ProximaNova_UI_2", "DDG_ProximaNova_UI_3", "DDG_ProximaNova_UI_4", "DDG_ProximaNova_UI_5", "DDG_ProximaNova_UI_6", "Proxima Nova", "Helvetica Neue", "Helvetica", "Segoe UI", "Nimbus Sans L", "Liberation Sans", "Open Sans", FreeSans, Arial, sans-serif;
    margin-top: 2px;
    font-size: 12px
}}

.mapview-marker__num.hide-on-unfocused {{
    display: none
}}

.has-focus .mapview-marker__num.hide-on-unfocused {{
    display: block
}}

.has-focus .mapview-marker__num.hide-on-focused {{
    display: none !important
}}

.mapview-marker__icn {{
    font-size: 16px
}}

.mapview-marker__icn.ddgsi-circle {{
    font-size: 8px
}}

.has-focus .mapview-marker__icn {{
    display: none !important
}}

.mapview-marker__icn--sel {{
    display: none
}}

.has-focus .mapview-marker__icn--sel {{
    display: block !important
}}

.mapview-marker__dot {{
    -moz-border-radius: 10px / 10px;
    -webkit-border-radius: 10px 10px;
    border-radius: 10px / 10px;
    display: none;
    position: absolute;
    background: #de5833;
    background-image: none;
    width: 10px;
    height: 10px;
    top: 30px;
    left: 8px
}}

.mapview-marker.mapview-marker-dot {{
    background-image: none
}}

.mapview-marker-dot .mapview-marker__dot {{
    display: block
}}

.mapview-marker__dot:hover,
.mapview-marker__dot.has-focus {{
    background: #333
}}

.mapview-marker.mapview-marker-dot .mapview-marker__num,
.mapview-marker.mapview-marker-dot .mapview-marker__icn {{
    display: none
}}

.mapview .leaflet-popup-content {{
    padding: 2px 10px
}}

.mapview-marker__popup {{
    font-weight: 600 !important;
    font-size: 12px;
    font-family: "DDG_ProximaNova", "DDG_ProximaNova_UI_0", "DDG_ProximaNova_UI_1", "DDG_ProximaNova_UI_2", "DDG_ProximaNova_UI_3", "DDG_ProximaNova_UI_4", "DDG_ProximaNova_UI_5", "DDG_ProximaNova_UI_6", "Proxima Nova", "Helvetica Neue", "Helvetica", "Segoe UI", "Nimbus Sans L", "Liberation Sans", "Open Sans", FreeSans, Arial, sans-serif;
    padding: 0;
    margin: 0;
    text-align: center;
    color: #333 !important;
    display: block;
    position: relative
}}

.mapview-marker__popup.mapview-marker__popup--single-line {{
    padding: 3px 0
}}

.mapview-marker__popup.has-directions {{
    padding-right: 30px
}}

.mapview-marker__popup .dropdown--directions {{
    position: absolute;
    top: 0;
    right: -10px;
    height: 100%;
    margin: 0;
    width: 30px
}}

.mapview-marker__popup .dropdown__button {{
    padding-right: 30px;
    height: 100%;
    color: #333
}}

.mapview-marker__popup .dropdown__button:hover {{
    color: #3a7fb4
}}

.mapview-marker__popup .dropdown__button:after {{
    height: 20px;
    width: 30px;
    margin-top: -10px;
    line-height: 20px;
    border-left: 1px solid #ededed;
    margin-left: 0
}}

.mapview-marker__popup__name {{
    display: block;
    font-size: 1.2em
}}

.mapview-marker__popup__latlon,
.mapview-marker__popup__address {{
    font-weight: normal !important;
    color: #888;
    line-height: 1;
    display: block;
    padding-bottom: 4px
}}

.mapview-marker__popup__name {{
    font-weight: 600;
    color: #333
}}

a:hover .mapview-marker__popup__name {{
    color: #3a7fb4
}}

.mapview-marker__popup__latlon,
.mapview-marker__popup__address {{
    color: #888
}}

a:hover .mapview-marker__popup__latlon,
a:hover .mapview-marker__popup__address {{
    color: #3a7fb4
}}

.mapview-marker__popup__directions-icon {{
    color: #4495d4;
    position: relative;
    top: 1px;
    right: 3px;
    display: none
}}

.has-directions .mapview-marker__popup__directions-icon {{
    display: inline-block
}}

.mapview-marker__popup__directions-name {{
    white-space: nowrap;
    overflow: hidden;
    text-overflow: ellipsis
}}

.mapview-marker__popup__directions-name .mapview-marker__popup__name {{
    display: inline
}}

.mapview-marker__popup:hover .mapview-marker__arrow {{
    background: #333;
    color: #fff;
    border-color: #333
}}

.mapview .zci__main--single-loc {{
    padding-top: 5px;
    pointer-events: none
}}

.is-mobile .mapview .zci__main--single-loc {{
    padding-left: 0;
    padding-top: 0px
}}

.mapview .zci__main--single-loc .tile__body {{
    min-height: 160px
}}

.tile--map {{
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s;
    -moz-box-shadow: 0 0 2px rgba(0, 0, 0, 0.2), 0 1px 0 rgba(0, 0, 0, 0.1);
    -webkit-box-shadow: 0 0 2px rgba(0, 0, 0, 0.2), 0 1px 0 rgba(0, 0, 0, 0.1);
    box-shadow: 0 0 2px rgba(0, 0, 0, 0.2), 0 1px 0 rgba(0, 0, 0, 0.1);
    margin-left: 18px;
    margin-left: 1.25rem;
    background-color: rgba(255, 255, 255, 0.96);
    pointer-events: auto;
    max-width: 32em;
    min-height: 7em;
    z-index: 25;
    border: none
}}

.tile--map .tile__body {{
    overflow: visible;
    padding-right: 128px
}}

.is-mobile .tile--map .tile__body {{
    font-size: 1em
}}

.tile--map .tile__title {{
    height: auto;
    margin: 0 14px 0 0;
    font-size: 1.4em;
    color: #333
}}

.tile--map .tile__title a:visited {{
    color: #333
}}

.tile--map .tile__sub {{
    margin: 0;
    height: auto;
    font-size: 1em;
    color: #333;
    white-space: initial;
    overflow: initial;
    text-overflow: initial;
    padding-top: 0.25em;
    padding-bottom: 0.25em
}}

.tile--map .tile__rating-and-price {{
    margin: .25em 0 .5em
}}

.tile--map .tile__rating-and-price .review-count {{
    vertical-align: baseline
}}

.tile--map .tile__rating {{
    margin-bottom: 0;
    display: inline
}}

.tile--map .tile__phone {{
    color: #333;
    padding-top: 0.25em
}}

.tile--map .tile__phone:hover {{
    color: #3a7fb4
}}

.tile--map .review-count__icon {{
    color: #aaa;
    margin-top: -3px
}}

.tile--map .tile__title a:hover,
.tile--map .tile__phone:hover,
.tile--map .tile__hours:hover {{
    color: #4495d4;
    text-decoration: underline
}}

.tile--map .tile__actions__btn {{
    border: none;
    display: inline-block;
    box-sizing: border-box;
    height: 30px;
    border-radius: 2px;
    padding-top: 2px;
    margin-top: 0.5em
}}

.tile--map .tile__actions__btn.btn {{
    background-color: #ededed;
    color: #333
}}

.tile--map .tile__actions__btn:hover {{
    background-color: #ededed;
    color: #3a7fb4
}}

.tile--map .tile__call {{
    margin-right: 0.5em;
    min-width: 80px
}}

.tile--map .tile__actions--small .tile__actions__btn {{
    padding-left: 0.5em;
    padding-right: 0.5em
}}

.tile--map .tile__actions--small .tile__call {{
    min-width: auto
}}

.tile--map .tile__directions {{
    display: inline-block
}}

.tile--map .tile__directions .tile__actions__btn {{
    border-top-right-radius: 0;
    border-bottom-right-radius: 0;
    margin-right: 2px
}}

.tile--map .dropdown--directions {{
    margin-right: 0;
    vertical-align: middle;
    height: auto;
    margin-top: 0.5em;
    font-size: 1em
}}

.tile--map .dropdown--directions .dropdown__button {{
    background-color: #ededed;
    padding-right: 20px;
    border-top-right-radius: 2px;
    border-bottom-right-radius: 2px
}}

.tile--map .dropdown--directions .dropdown__button:after {{
    color: #333;
    margin-left: 4px;
    margin-top: -6px
}}

.tile--map .dropdown--directions .dropdown__button:hover:after {{
    color: #3a7fb4
}}

.tile--map__side {{
    position: absolute;
    top: 0;
    right: 0;
    height: 100%;
    border-left: 1px solid #eee;
    padding: 0 14px;
    width: 100px
}}

.tile--map__side .tile__media {{
    margin-top: 14px;
    width: 8em;
    height: 8em;
    background: #fafafa;
    text-align: center;
    text-decoration: none !important
}}

.is-mobile .tile--map__side .tile__media {{
    width: 86px;
    height: 86px
}}

.tile--map__side .tile__media__no-img {{
    display: block;
    font-size: 4em;
    line-height: 2;
    height: 2em;
    color: #ccc;
    background: #e6e6e6;
    padding: 0
}}

.is-mobile .tile--map__side .tile__media__no-img {{
    font-size: 64px;
    line-height: 86px;
    height: 86px
}}

.tile--map__side .tile__more-wrap {{
    position: absolute;
    bottom: 16px;
    font-size: 0.9em
}}

.tile--map__main {{
    min-height: 7em;
    margin: 0 14px 0 0
}}

.tile--map__main.has-hours {{
    position: relative;
    min-height: 8.5em
}}

.tile--map__main__not-hours {{
    opacity: 1;
    visibility: visible
}}

.is-showing-hours .tile--map__main__not-hours {{
    opacity: 0;
    visibility: hidden
}}

.tile--map--mob {{
    -moz-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    -webkit-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
    pointer-events: auto;
    border-bottom: 1px solid #dbdbdb;
    border-bottom-color: transparent;
    margin-top: -1px;
    margin-left: 0;
    margin-right: 0;
    background-color: #f2f2f2;
    position: relative;
    max-width: initial;
    z-index: 1000;
    margin-bottom: 150px
}}

.tile--map--mob .tile__body {{
    padding-right: 114px
}}

.tile--map--mob .tile--map__main {{
    min-height: 94px
}}

.tile--map--mob .tile--map__side {{
    width: 86px;
    border-color: #e6e6e6
}}

.tile--map--mob .tile--map__side .tile__media {{
    margin-top: 14px
}}

.tile--map--mob .tile__hours-full {{
    border-color: #e6e6e6;
    background-color: #f2f2f2
}}

.tile--map--mob .tile__hours-today__expand {{
    display: inline-block
}}

.tile--map--mob .tile__actions__btn.btn,
.tile--map--mob .dropdown--directions .dropdown__button {{
    background-color: #e0e0e0
}}

.tile--map--mob .tile__actions__btn.btn:hover,
.tile--map--mob .dropdown--directions .dropdown__button:hover {{
    background-color: #e0e0e0
}}

.tile--loc {{
    width: 9.25em;
    height: 15em;
    margin: 0 0.25em;
    color: #888
}}

.tile--loc .tile__num {{
    display: none
}}

.tile--loc .tile__media {{
    overflow: hidden;
    text-align: center;
    padding: .5em 0;
    width: 8em;
    height: 8em
}}

.tile--loc .tile__media__wrapper {{
    width: 100%;
    height: 100%;
    overflow: hidden;
    background: #fafafa
}}

.tile--loc .tile__media__img {{
    min-width: 100%;
    max-width: 150%;
    min-height: 8em
}}

.tile--loc .tile__media__no-img {{
    display: block;
    font-size: 4em;
    color: #ccc;
    background: #e6e6e6;
    padding: 0;
    height: 2em;
    line-height: 2
}}

.tile--loc .tile__title {{
    font-size: 1em;
    font-weight: 600;
    max-height: 2.6em;
    height: auto;
    margin-bottom: 0
}}

.tile--loc .tile__body {{
    font-size: 1em;
    padding: 0 0.5em 0.5em
}}

.tile--loc .tile__foot {{
    word-wrap: normal;
    padding-left: 0.5em;
    padding-right: 0.5em;
    padding-bottom: 0.6em;
    font-size: 0.9em;
    height: 1.3em;
    margin-bottom: 0
}}

.tile--loc .tile__rating {{
    overflow: visible;
    line-height: 1.4;
    height: 1.3em
}}

.tile--loc .review-count {{
    color: #aaa;
    vertical-align: bottom;
    line-height: 1.2
}}

.tile--loc .review-count__icon {{
    display: inline;
    vertical-align: bottom
}}

.tile--loc__alt .tile__body {{
    font-size: 1em;
    padding: 0.5em;
    overflow: visible
}}

.tile--loc__alt .tile__body.has-segments {{
    padding: 0
}}

.tile--loc__alt .tile__body.has-segments .tile__segment {{
    padding: 0.5em
}}

.tile--loc__alt .tile__body.has-segments .tile__segment__title {{
    padding-top: 2px
}}

.tile--loc__alt .tile__price {{
    display: block
}}

.tile--loc__alt .tile__title {{
    display: block;
    margin-bottom: 0.5em
}}

.tile--loc__alt .tile__phone {{
    color: #888;
    font-size: 0.9em
}}

.tile--loc__alt .tile__phone:hover {{
    color: #3a7fb4;
    text-decoration: underline
}}

.tile--loc__alt .tile__call:hover .tile__segment__title {{
    color: #3a7fb4;
    text-decoration: underline
}}

.tile--loc__alt .tile__call .tile__phone:hover {{
    color: #888;
    text-decoration: none
}}

.tile--loc__alt .tile__title a:hover {{
    color: #4495d4;
    text-decoration: underline
}}

.tile--loc__alt .tile__directions__link {{
    height: 32px;
    width: 75%;
    z-index: 1;
    box-sizing: border-box;
    position: relative;
    display: block
}}

.tile--loc__alt .dropdown--directions {{
    width: 100%;
    position: absolute;
    height: 100%;
    top: 0;
    left: 0;
    padding-left: 0.5em;
    font-size: 1em
}}

.tile--loc__alt .dropdown__button {{
    width: 80px;
    padding-top: 1.2em;
    padding-right: 25px;
    font-size: 0.9em;
    color: #888;
    text-overflow: ellipsis;
    white-space: nowrap;
    overflow: hidden
}}

.tile--loc__alt .dropdown__button:after {{
    color: #333;
    position: absolute;
    right: 1em;
    height: 15px;
    padding-left: 5px;
    border-left: 1px solid rgba(0, 0, 0, 0.08);
    top: 1.5em;
    padding-top: 2px
}}

.tile--loc__more {{
    color: #888
}}

.tile--loc__more:hover {{
    color: #4495d4;
    text-decoration: underline
}}

.tile--loc__more__icon {{
    vertical-align: middle;
    padding-right: 4px;
    margin-top: -2px;
    max-width: 1.25em
}}

.is-expanded .tile--loc .tile__num {{
    display: block
}}

.tile__rating__foursquare {{
    background: #e0e0e0;
    color: #333;
    padding: 0 5px
}}

.tile__rating__foursquare.badge--txt {{
    font-size: inherit;
    line-height: inherit;
    margin-top: 0
}}

.tile__rating__foursquare.foursquare--high {{
    background: #5b9e4d;
    color: #fff
}}

.tile__rating__foursquare.foursquare--med {{
    background: #f1a031;
    color: #fff
}}

.tile__rating__foursquare.foursquare--low {{
    background: #de5833;
    color: #fff
}}

.tile--loc .tile__rating__foursquare {{
    float: right
}}

.tile--map .tile__rating__foursquare {{
    margin-right: 8px
}}

.tile__rating__yelp-stars {{
    display: inline-block;
    vertical-align: top;
    width: 70px;
    height: 14px;
    margin-right: 5px;
    margin-top: 1px
}}

.tile--loc .tile__rating__yelp-stars {{
    vertical-align: middle;
    margin-top: -1px
}}

.modal--dropdown--directions .modal__header {{
    padding: 0.75em 1em
}}

.modal--dropdown--directions .modal__box {{
    width: 160px;
    left: -80px
}}

.modal--dropdown--directions .modal__list__link {{
    padding-left: 0.5em
}}

.modal--dropdown--directions.modal--popout--bottom .modal__box:before,
.modal--dropdown--directions.modal--popout--bottom .modal__box:after,
.modal--dropdown--directions.modal--popout--bottom-right .modal__box:before,
.modal--dropdown--directions.modal--popout--bottom-right .modal__box:after,
.modal--dropdown--directions.modal--popout--bottom-left .modal__box:before,
.modal--dropdown--directions.modal--popout--bottom-left .modal__box:after {{
    content: "\25b2"
}}

.modal--dropdown--directions.modal--popout--top .modal__box:before,
.modal--dropdown--directions.modal--popout--top .modal__box:after {{
    content: "\25bc"
}}

.leaflet-control-layers,
.leaflet-bar {{
    -moz-box-shadow: 0 0 1px 1px rgba(0, 0, 0, 0.15);
    -webkit-box-shadow: 0 0 1px 1px rgba(0, 0, 0, 0.15);
    box-shadow: 0 0 1px 1px rgba(0, 0, 0, 0.15);
    border: none
}}

.mapview .leaflet-bottom,
.mapview .leaflet-top {{
    z-index: 14
}}

.mapview .leaflet-bottom .leaflet-control {{
    clear: none
}}

.leaflet-control-expandmap {{
    font-size: 14px;
    text-align: center;
    padding: 7px 0 6px;
    line-height: 1
}}

.has-tileview .leaflet-bottom {{
    margin-bottom: 17em
}}

.has-tileview .leaflet-top {{
    margin-top: 47px
}}

.mapview .mapbox-control-info-right {{
    margin-top: 2px
}}

.mapview .mapbox-control-info-right .map-info-container {{
    -moz-border-radius: 13px / 13px;
    -webkit-border-radius: 13px 13px;
    border-radius: 13px / 13px;
    padding-left: 10px;
    color: #888;
    background: rgba(255, 255, 255, 0.5)
}}

.mapview .mapbox-control-info-right .map-info-container a,
.mapview .mapbox-control-info-right .map-info-container a:hover {{
    color: #4495d4
}}

.not-mobile.mapbox-control-info-right .map-info-container {{
    padding: 2px 10px
}}

.not-mobile .mapbox-info-toggle {{
    display: none
}}

.controls-hidden .leaflet-control-container,
.controls-hidden .mapview__map__attribution {{
    display: none
}}

.tile--img {{
    -moz-box-shadow: none !important;
    -webkit-box-shadow: none !important;
    box-shadow: none !important;
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s;
    display: inline-block;
    text-align: center;
    min-width: 60px;
    line-height: 159.74px;
    height: 163px;
    width: auto;
    background: none;
    border: none
}}

.is-mobile .tile--img {{
    line-height: 117.6px;
    height: 120px;
    min-width: 48px
}}

.is-mobile .has-tiles--grid .tile--img {{
    margin-top: 2px;
    margin-left: 3px;
    margin-right: 3px
}}

.tile--img.highlight .tile--img__details {{
    visibility: visible;
    opacity: 1
}}

.tile--img.is-selected .tile--img__details {{
    visibility: hidden;
    opacity: 0
}}

.tile--img.is-selected:after,
.tile--img.is-selected:before {{
    border-bottom-width: 8px;
    visibility: visible;
    opacity: 1
}}

.tile--img:after,
.tile--img:before {{
    position: absolute;
    visibility: hidden;
    display: block;
    margin: auto;
    content: "";
    opacity: 0;
    z-index: 4;
    height: 0;
    width: 0;
    right: 0;
    left: 0;
    border-left: 8px solid transparent;
    border-right: 8px solid transparent;
    border-top: 0
}}

.tile--img:after {{
    bottom: 0;
    border-bottom: 0px solid #f2f2f2
}}

.tileview--grid .tile--img:active:after {{
    border-top-color: #de5833
}}

.tile--m--images {{
    display: inline-block;
    width: 163px;
    height: 163px
}}

.is-mobile .tile--m--images {{
    height: 120px
}}

.tileview--grid .tile--m--images {{
    margin-left: 3px;
    margin-right: 3px
}}

.has-tiles--grid .tile--m--images {{
    -webkit-appearance: none;
    -moz-appearance: none;
    -ms-appearance: none;
    -o-appearance: none;
    appearance: none;
    -moz-transition: none 0.3s ease-in-out 0s;
    -o-transition: none 0.3s ease-in-out 0s;
    -webkit-transition: none 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: none 0.3s ease-in-out 0s;
    -moz-border-radius: 0.25em;
    -webkit-border-radius: 0.25em;
    border-radius: 0.25em;
    outline: none !important;
    text-decoration: none !important;
    background-color: #fafafa;
    border: 1px solid #ddd;
    cursor: pointer;
    -webkit-user-select: none;
    -khtml-user-select: none;
    -moz-user-select: -moz-none;
    -ms-user-select: none;
    user-select: none;
    height: auto;
    display: block;
    font-weight: 600;
    padding: 0;
    margin: 10px auto 18px;
    font-size: 1em;
    line-height: 3
}}

.has-tiles--grid .tile--m--images .hide--mob {{
    display: none
}}

.has-tiles--grid .tile--m--images .tile--m--mob {{
    display: block;
    text-transform: capitalize
}}

.zci .zci--images {{
    padding-left: 0;
    padding-right: 0
}}

.tile--img__media {{
    overflow: hidden;
    max-height: 100%;
    max-width: 100%;
    display: block
}}

.tile--img__media:after {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    background-color: rgba(0, 0, 0, 0.5);
    visibility: hidden;
    opacity: 0;
    content: ""
}}

.is-selected .tile--img__media:after {{
    visibility: visible;
    opacity: 1
}}

.tile--img__media__i {{
    position: relative;
    display: inline-block;
    right: -50%;
    bottom: -50%
}}

.tile--img__media__i .tile--img__img {{
    position: relative;
    left: -50%;
    top: -50%
}}

.tile--img__img {{
    text-indent: -9999px;
    vertical-align: middle;
    height: 163px;
    max-width: none;
    width: auto;
    color: transparent
}}

.tile--img__img:-moz-loading {{
    visibility: hidden
}}

.is-mobile .tile--img__img {{
    height: 120px
}}

.tile--img__details {{
    opacity: 0;
    color: #ffffff;
    background: #595959;
    background: rgba(0, 0, 0, 0.5);
    visibility: hidden;
    text-align: center;
    position: absolute;
    height: 100%;
    width: 100%;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0
}}

.touch .tile--img__details {{
    display: none
}}

.tile--img__dimensions {{
    vertical-align: middle;
    display: inline-block;
    line-height: normal
}}

.tile--img__dimensions i,
.tile--img__dimensions em {{
    font-style: normal
}}

.tile--img__dimensions em {{
    font-weight: 600;
    display: block
}}

.tile--img__icon {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
    display: inline-block;
    border: 2px solid white;
    margin-bottom: 12px;
    line-height: 36px;
    font-size: 16px;
    height: 36px;
    width: 36px
}}

.tileview__images {{
    min-height: 176px
}}

.tileview__images.has-tiles--grid {{
    margin-left: 0
}}

@media only screen and (max-width: 590px) {{
    .tileview__images.has-tiles--grid {{
        padding-right: 0
    }}
}}

.is-mobile .tileview__images {{
    min-height: 177px
}}

.is-mobile .detail__media--images {{
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0;
    max-height: 100% !important
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail__media--images {{
        width: 100% !important
    }}
}}

.is-mobile .detail__body--images .c-detail__title {{
    display: none
}}

.is-mobile .detail__body--images .c-detail__desc p {{
    color: #fff;
    font-size: 1.1em
}}

.is-mobile .detail__body--images .c-detail__filemeta {{
    color: rgba(200, 200, 200, 0.8)
}}

.is-mobile .detail__body--images .sep {{
    border-color: rgba(175, 175, 175, 0.25);
    margin: 0 1em
}}

@media only screen and (max-width: 590px) {{
    .has-tiles .tile--img {{
        width: auto !important
    }}
}}

.tile--vid {{
    width: 18.35em
}}

.tile--vid .tile__media {{
    padding-bottom: 56.25%
}}

.tile--vid .tile__body {{
    padding-top: 0.8em;
    padding-bottom: 0.8em
}}

.is-mobile .tile--vid .tile__media {{
    background-color: #000;
    padding-bottom: 8.5em
}}

.tile--vid__dur {{
    font-weight: 600;
    background-color: #595959;
    background-color: rgba(0, 0, 0, 0.6);
    padding: 0.125em 0.5em;
    display: inline-block;
    position: absolute;
    bottom: 0.5em;
    right: 0.8em;
    line-height: 1.6;
    font-size: 0.9em;
    color: #fff
}}

.tile--vid__overlay {{
    -moz-transition: ease-in-out 0.1s 0s;
    -o-transition: ease-in-out 0.1s 0s;
    -webkit-transition: ease-in-out 0.1s;
    -webkit-transition-delay: 0s;
    transition: ease-in-out 0.1s 0s;
    background-color: rgba(0, 0, 0, 0.7);
    visibility: hidden;
    opacity: 0
}}

.highlight .tile--vid__overlay {{
    visibility: visible;
    opacity: 1
}}

.tile--vid__overlay__icon {{
    -moz-box-sizing: border-box;
    -webkit-box-sizing: border-box;
    box-sizing: border-box;
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
    border: 4px solid white;
    text-align: center;
    position: absolute;
    font-size: 36px;
    font-size: 2.5rem;
    font-style: normal;
    line-height: 1.5;
    margin-left: -0.75em;
    margin-top: -0.75em;
    text-indent: 3px;
    display: block;
    color: white;
    opacity: 0.9;
    height: 1.5em;
    width: 1.5em;
    left: 50%;
    top: 50%
}}

.tile--m--videos {{
    font-size: 0.75em;
    width: 18.35em
}}

.tile--m--videos:before {{
    content: "";
    display: block;
    padding-bottom: 56.25%;
    height: 0
}}

.is-mobile .tile--m--videos:before {{
    display: none
}}

.tile--m--videos .tile__body {{
    height: 7em
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .tile--m--videos .tile__body {{
        height: 5.8em
    }}
}}

@media only screen and (max-width: 864px) {{
    .detail--videos .detail__body {{
        display: none
    }}
}}

.is-mobile .detail--videos .detail__body {{
    position: static;
    padding-top: 3em
}}

@media only screen and (max-width: 864px) {{
    .is-mobile .detail--videos .detail__body {{
        display: block
    }}
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail--videos .detail__body {{
        padding-top: 0;
        display: table-cell;
        width: 100%
    }}
}}

.is-mobile .detail--videos.has-privacy-warning .detail__close {{
    color: #fff
}}

.is-mobile .detail--videos.has-privacy-warning .detail__controls {{
    z-index: -1
}}

.detail__media--vid {{
    margin-right: 2.2em
}}

.is-mobile .detail .detail__media--vid {{
    height: auto;
    margin-left: 0;
    margin-right: 0
}}

@media only screen and (min-width: 590px) {{
    .is-mobile .detail .detail__media--vid {{
        margin-right: 2.2em;
        float: left
    }}
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail .detail__media--vid {{
        margin-right: 0;
        float: none
    }}
}}

.detail__media__vid-wrap {{
    position: absolute;
    display: block;
    margin: auto;
    width: 100%;
    height: 100%;
    bottom: 0;
    left: 0;
    top: 0;
    background-size: cover;
    background-position: center
}}

.is-expanded .detail__media__vid-wrap {{
    right: 0
}}

.is-mobile .detail .detail__media__vid-wrap {{
    position: relative
}}

.overlay--video-privacy {{
    font-size: .85em
}}

.is-mobile .overlay--video-privacy {{
    font-size: 1em
}}

@media only screen and (min-width: 1079px) {{
    .overlay--video-privacy {{
        font-size: 1em
    }}
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .overlay--video-privacy {{
        font-size: .85em
    }}
}}

@media only screen and (max-width: 590px) {{
    .overlay--video-privacy {{
        font-size: .8em
    }}
}}

@media only screen and (max-width: 425px) {{
    .overlay--video-privacy {{
        font-size: .6em
    }}
}}

.overlay--video-privacy__icon {{
    display: block;
    width: 60px;
    height: 60px;
    position: relative;
    margin: 0 auto;
    background: url("/img/privacy.png") 50% 50% no-repeat;
    background-size: 60px 60px
}}

.overlay--video-privacy__text {{
    padding-right: 15%;
    padding-left: 15%;
    font-size: 1.2em
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .overlay--video-privacy__text {{
        padding-right: 10%;
        padding-left: 10%
    }}
}}

@media only screen and (max-width: 864px) {{
    .overlay--video-privacy__text {{
        padding-right: 10%;
        padding-left: 10%
    }}
}}

@media only screen and (max-width: 425px) {{
    .overlay--video-privacy__text {{
        padding-right: 0.5em;
        padding-left: 0.5em
    }}
}}

.is-mobile .overlay--video-privacy__text {{
    padding-right: 1.25em;
    padding-left: 1.25em;
    font-size: 0.9em
}}

.overlay--video-privacy__remember {{
    color: #bbb;
    font-weight: 300
}}

.overlay--video-privacy__remember a {{
    color: #49a9f2
}}

.overlay--video-privacy__remember input {{
    margin-right: 0.5em;
    vertical-align: middle
}}

.is-mobile .overlay--video-privacy__remember {{
    display: inline-block;
    margin-top: 1em;
    font-size: 0.8em
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    .overlay--video-privacy__icon {{
        background-image: url("/img/privacy@2x.png")
    }}
}}

.zci--videos {{
    min-height: 272px;
    min-height: 18.88889rem
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .zci--videos {{
        min-height: 227px;
        min-height: 15.76389rem
    }}
}}

.is-mobile .zci--videos {{
    min-height: 247px;
    min-height: 17.15278rem
}}

.tile--products {{
    width: 16em
}}

.tile--pr .review-count {{
    white-space: nowrap;
    word-wrap: normal;
    max-width: 2em
}}

.tile--pr .stars {{
    margin-left: -1px
}}

.tile--m--products,
.tile--m--isbn {{
    width: 16em;
    height: 197px
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .tile--m--products,
    .tile--m--isbn {{
        height: 13.9em
    }}
}}

.tile--pr--w {{
    max-width: 100%;
    width: 40em
}}

.tile--pr--w .tile__media {{
    float: left;
    width: 16em
}}

.tile__media--pr {{
    background-color: #fff;
    position: relative;
    height: 9.6em
}}

.tile__media--pr .tile__media__img {{
    position: absolute;
    margin: auto;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0;
    max-height: 92%;
    z-index: 0
}}

.tile__media--pr:after {{
    background: url('data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0idXRmLTgiPz4gPHN2ZyB2ZXJzaW9uPSIxLjEiIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyI+PGRlZnM+PGxpbmVhckdyYWRpZW50IGlkPSJncmFkIiBncmFkaWVudFVuaXRzPSJvYmplY3RCb3VuZGluZ0JveCIgeDE9IjAuNSIgeTE9IjAuMCIgeDI9IjAuNSIgeTI9IjEuMCI+PHN0b3Agb2Zmc2V0PSIwJSIgc3RvcC1jb2xvcj0iIzAwMDAwMCIgc3RvcC1vcGFjaXR5PSIwLjAiLz48c3RvcCBvZmZzZXQ9IjEwMCUiIHN0b3AtY29sb3I9IiMwMDAwMDAiIHN0b3Atb3BhY2l0eT0iMC4wMyIvPjwvbGluZWFyR3JhZGllbnQ+PC9kZWZzPjxyZWN0IHg9IjAiIHk9IjAiIHdpZHRoPSIxMDAlIiBoZWlnaHQ9IjEwMCUiIGZpbGw9InVybCgjZ3JhZCkiIC8+PC9zdmc+IA==');
    background: -webkit-gradient(linear, 50% 0%, 50% 100%, color-stop(0%, rgba(0, 0, 0, 0)), color-stop(100%, rgba(0, 0, 0, 0.03)));
    background: -moz-linear-gradient(top, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.03));
    background: -webkit-linear-gradient(top, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.03));
    background: linear-gradient(to bottom, rgba(0, 0, 0, 0), rgba(0, 0, 0, 0.03));
    content: ""
}}

.tile__body--pr {{
    padding-top: 8.64px;
    padding-top: 0.6rem;
    padding-bottom: 8.64px;
    padding-bottom: 0.6rem
}}

.tile--pr__sub {{
    margin-bottom: 0.125em;
    color: #494949;
    height: 1.325em
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .has-rating .tile--pr__sub {{
        overflow: visible;
        height: 0;
        margin: 0
    }}
    .has-rating .tile--pr__sub .tile--pr__price {{
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis;
        bottom: 8.64px;
        bottom: 0.6rem;
        right: 0.8em;
        position: absolute;
        vertical-align: top;
        max-width: 4.3em;
        margin-bottom: -0.25em;
        line-height: 1.5
    }}
    .has-rating .tile--pr__sub .tile__sep,
    .has-rating .tile--pr__sub .tile--pr__brand {{
        display: none
    }}
}}

.tile--pr__badge {{
    -moz-border-radius: 2px;
    -webkit-border-radius: 2px;
    border-radius: 2px;
    background-color: rgba(255, 255, 255, 0.8);
    padding: 0.5em;
    margin: 0.5em
}}

.tile__title--pr {{
    margin-bottom: 0.5em;
    font-size: 1.08em
}}

@media only screen and (max-width: 590px) {{
    .detail--products .detail__desc {{
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis;
        max-width: 100%
    }}
}}

.detail__media--pr {{
    margin-right: 2em;
    border-right: 1px solid #e3e3e3;
    width: 37%
}}

@media only screen and (max-width: 590px) {{
    .detail__media--pr {{
        margin-right: 0;
        width: 100%
    }}
}}

.is-mobile .detail__media--pr {{
    max-height: 40%;
    border: none;
    height: 40%
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail__media--pr {{
        min-width: 120px
    }}
}}

.is-mobile .detail__body--pr {{
    position: static;
    padding-top: 3em;
    top: 40%
}}

.is-mobile .detail__body--pr .detail__body__content {{
    display: block;
    padding-right: 0
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail__body--pr {{
        padding-top: 0
    }}
    .is-mobile .detail__body--pr .detail__body__content {{
        max-width: 17em
    }}
}}

.detail__callout--pr {{
    display: block;
    margin-top: 1em
}}

.is-mobile .detail .detail__callout--pr {{
    position: absolute;
    bottom: 1.25em;
    left: 1.25em
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    .is-mobile .detail .detail__callout--pr {{
        left: auto
    }}
}}

.tileview__products {{
    min-height: 212px
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .tileview__products {{
        min-height: 12.7em
    }}
}}

.tile--news {{
    height: 19.5em;
    width: 20em;
    overflow: hidden
}}

.tile--news .tile__title {{
    font-size: 1.35em;
    font-weight: 600;
    line-height: 1.3;
    overflow: hidden;
    height: auto;
    max-height: 5.2em;
    display: block
}}

.tile--news .tile__title.tile__title--under-image {{
    max-height: 3.9em
}}

.tile--news .tile__media {{
    background: #f7f7f7;
    height: 10em
}}

.tile--news .tile__media__img {{
    background-size: cover;
    width: 100%;
    height: 100%;
    position: relative
}}

.tile--news .tile__body {{
    height: 100%
}}

.tile__content--news {{
    font-size: 1.05em;
    line-height: 1.5;
    max-height: 6em;
    height: 6em;
    overflow: hidden
}}

.tile__foot--news {{
    color: #888;
    background: #fff
}}

.zci--news {{
    min-height: 298px;
    min-height: 20.69444rem
}}

.is-mobile .zci--news {{
    min-height: 180px;
    min-height: 12.5rem
}}

.is-mobile .tile--news .tile__body {{
    font-size: 1em
}}

.is-mobile .tile--news .tile__media {{
    height: 11em
}}

.is-mobile .tile--news .tile__body.has-foot {{
    padding-bottom: 2.8em
}}

@media only screen and (max-width: 425px) {{
    .is-mobile .tile--news {{
        height: auto
    }}
}}

.tileview__apps.has-tiles .tile--pr {{
    width: 12.25em
}}

.tile--apps .tile__media--pr {{
    height: 132px
}}

.tile--apps .tile__body {{
    padding-top: 0.7em
}}

.tile--apps .tile__media__img {{
    width: 100px
}}

.detail--apps .detail__media__img {{
    max-height: 96%
}}

.detail--apps .detail__media--pr {{
    background: none;
    margin-right: 2.1em
}}

.detail--apps .detail__media--pr:after {{
    display: none
}}

@media only screen and (max-width: 590px) {{
    .detail--apps .detail__media--pr {{
        margin-right: auto
    }}
}}

@media only screen and (min-width: 590px) and (max-height: 738px) {{
    .tile--apps .tile--pr__sub {{
        display: none
    }}
}}

.tile--qa {{
    height: 16em;
    width: 18.5em
}}

.is-mobile .tile--qa {{
    height: auto
}}

.tile--qa h2,
.tile--qa pre {{
    display: none
}}

.tile--qa .tile__body {{
    height: 100%
}}

.is-mobile .tile--qa .tile__body {{
    font-size: 1em
}}

.tile__content--qa {{
    max-height: 5.6em;
    overflow: hidden
}}

.tile__content--qa p {{
    padding: 0
}}

.detail__body--qa {{
    padding-left: 0
}}

.is-mobile .detail.detail--qa .detail__pane,
.is-mobile .detail.detail--about .detail__pane {{
    margin-top: 3em;
    padding-top: 0
}}

.is-mobile .detail .detail__body--qa {{
    padding-right: 1em;
    padding-left: 1em;
    padding-top: 1.25em;
    position: static
}}

.is-mobile .detail .detail__body--qa .detail__body__content--qa {{
    padding-right: 0;
    max-width: 100%
}}

.is-mobile .detail .detail__body--qa .detail__body__content--qa .chomp--scroll {{
    max-height: initial;
    padding-bottom: 4em
}}

.is-mobile .detail .detail__body--qa .detail__more {{
    position: absolute;
    top: -0.25em;
    left: 1em
}}

.region-flag__wrap {{
    position: relative;
    display: block;
    float: left
}}

.region-flag__img {{
    display: inline-block;
    vertical-align: top;
    position: relative
}}

.region-flag__wrap--small {{
    width: 20px;
    height: 20px
}}

.region-flag__wrap--small.has-region:before {{
    content: '';
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    border: 1px solid #000;
    position: absolute;
    opacity: 0.2;
    border-radius: 100%;
    z-index: 1
}}

.region-flag__wrap--small .region-flag__img {{
    height: 20px
}}

.region-flag__wrap--xsmall {{
    width: 16px;
    height: 16px
}}

.region-flag__wrap--xsmall.has-region:before {{
    content: '';
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    border: 1px solid #000;
    position: absolute;
    opacity: 0.2;
    border-radius: 100%;
    z-index: 1
}}

.region-flag__wrap--xsmall .region-flag__img {{
    height: 16px
}}

.region-flag__wrap--large {{
    height: 32px;
    width: 32px
}}

.region-flag__wrap--large.has-region:before {{
    content: '';
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    border: 2px solid #000;
    position: absolute;
    opacity: 0.2;
    border-radius: 100%;
    z-index: 1
}}

.region-flag__wrap--large .region-flag__img {{
    height: 32px
}}

.tile--audio {{
    width: 14em
}}

.tile--audio .tile__media {{
    padding-bottom: 100%;
    height: 0;
    background-color: #f7f7f7
}}

.tile--audio .tile__media__img {{
    max-width: none;
    height: 100%;
    position: absolute;
    margin: auto;
    right: 0;
    left: 0;
    top: 0
}}

.tile--audio .tile__title {{
    font-size: 1.1em;
    height: 2.4em;
    line-height: 1.2em;
    display: block
}}

.tile--audio .tile__footer {{
    height: auto;
    margin-bottom: 0;
    margin-top: 1em;
    color: #aaa;
    line-height: 1em
}}

.audio-controls {{
    position: absolute;
    display: block;
    opacity: 0;
    width: 100%;
    height: 100%;
    top: 0;
    left: 0;
    right: 0;
    bottom: 0;
    text-align: center
}}

.is-paused .audio-controls,
.is-selected .audio-controls,
.highlight .audio-controls,
.is-unavailable .audio-controls {{
    opacity: 1;
    background: rgba(0, 0, 0, 0.7)
}}

.audio-controls__action {{
    display: block;
    position: absolute;
    left: 0;
    top: 50%;
    font-size: 2.8em;
    line-height: 2.8em !important;
    margin-top: -1.4em;
    width: 100%;
    color: #fff
}}

.is-loading .audio-controls__action,
.is-unavailable .audio-controls__action {{
    display: none
}}

.audio-controls__progress {{
    position: absolute;
    height: 100%;
    width: 100%;
    left: 0;
    top: 0;
    right: 0;
    bottom: 0
}}

.audio-controls__progress-bg,
.audio-controls__progress-loading,
.audio-controls__progress-playback {{
    position: absolute;
    margin: -4em 0 0 -4em;
    top: 50%;
    left: 50%;
    width: 8em;
    height: 8em;
    clip: rect(0, 8em, 8em, 4em);
    z-index: 2
}}

.is-unavailable .audio-controls__progress-bg,
.is-unavailable .audio-controls__progress-loading,
.is-unavailable .audio-controls__progress-playback {{
    display: none
}}

.audio-controls__progress-bg.gt50,
.audio-controls__progress-loading.gt50,
.audio-controls__progress-playback.gt50 {{
    clip: rect(auto, auto, auto, auto)
}}

.audio-controls__progress-bg.gt50 .full-fill,
.audio-controls__progress-loading.gt50 .full-fill,
.audio-controls__progress-playback.gt50 .full-fill {{
    display: block
}}

.audio-controls__progress-bg .rotated-fill,
.audio-controls__progress-bg .full-fill,
.audio-controls__progress-loading .rotated-fill,
.audio-controls__progress-loading .full-fill,
.audio-controls__progress-playback .rotated-fill,
.audio-controls__progress-playback .full-fill {{
    display: block;
    position: absolute;
    border: .4em solid #fff;
    width: 7.2em;
    height: 7.2em;
    clip: rect(0em, 4em, 8em, 0em);
    -moz-border-radius: 4em;
    -webkit-border-radius: 4em;
    border-radius: 4em
}}

.audio-controls__progress-bg .full-fill,
.audio-controls__progress-loading .full-fill,
.audio-controls__progress-playback .full-fill {{
    -webkit-transform: rotate(180deg);
    -moz-transform: rotate(180deg);
    -o-transform: rotate(180deg);
    transform: rotate(180deg);
    display: none
}}

.audio-controls__progress-loading .rotated-fill,
.audio-controls__progress-loading .full-fill {{
    border-color: rgba(255, 255, 255, 0.2)
}}

.is-loading .audio-controls__progress-loading {{
    clip: rect(auto, auto, auto, auto)
}}

.is-loading .audio-controls__progress-loading .rotated-fill {{
    -webkit-animation: spin 2s linear 0s infinite;
    -moz-animation: spin 2s linear 0s infinite;
    -o-animation: spin 2s linear 0s infinite;
    animation: spin 2s linear 0s infinite
}}

.is-selected .audio-controls__progress-bg {{
    clip: rect(auto, auto, auto, auto)
}}

.is-selected .audio-controls__progress-bg .full-fill {{
    display: block
}}

.is-selected .audio-controls__progress-bg .rotated-fill,
.is-selected .audio-controls__progress-bg .full-fill {{
    border-color: rgba(255, 255, 255, 0.1)
}}

.is-selected.is-loading .audio-controls__progress-bg .rotated-fill,
.is-selected.is-loading .audio-controls__progress-bg .full-fill {{
    display: none
}}

.audio-controls__time {{
    position: absolute;
    color: #fff;
    bottom: 1em;
    right: 1em
}}

.is-unavailable .audio-controls__time {{
    color: #ccc;
    font-size: .9em;
    width: 100%;
    text-align: center;
    right: 0
}}

@-moz-keyframes spin {{
    from {{
        -moz-transform: rotate(0deg)
    }}
    to {{
        -moz-transform: rotate(360deg)
    }}
}}

@-webkit-keyframes spin {{
    from {{
        -webkit-transform: rotate(0deg)
    }}
    to {{
        -webkit-transform: rotate(360deg)
    }}
}}

@-o-keyframes spin {{
    from {{
        -o-transform: rotate(0deg)
    }}
    to {{
        -o-transform: rotate(360deg)
    }}
}}

@keyframes spin {{
    from {{
        transform: rotate(0deg)
    }}
    to {{
        transform: rotate(360deg)
    }}
}}

.feedback-btn {{
    -moz-transition: height 0.2s ease-in-out 0.05s;
    -o-transition: height 0.2s ease-in-out 0.05s;
    -webkit-transition: height 0.2s ease-in-out;
    -webkit-transition-delay: 0.05s;
    transition: height 0.2s ease-in-out 0.05s;
    background-color: #ededed;
    border: none;
    padding: 0;
    height: 28px;
    min-width: 120px;
    text-align: center;
    margin: 0.5em 1em;
    box-sizing: border-box
}}

.feedback-btn:hover {{
    background-color: #ededed
}}

.results__sidebar .feedback-btn {{
    float: left;
    margin-top: 30px;
    margin-left: 0
}}

@media only screen and (max-width: 1130px) {{
    .has-right-rail-ads .results--sidebar .feedback-btn {{
        display: none
    }}
}}

.feedback-btn--clicked {{
    -moz-transition-delay: 0s;
    -o-transition-delay: 0s;
    -webkit-transition-delay: 0s;
    transition-delay: 0s;
    height: 42px
}}

.feedback-btn__send {{
    -moz-transition: opacity 0.2s ease-in-out 0s;
    -o-transition: opacity 0.2s ease-in-out 0s;
    -webkit-transition: opacity 0.2s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: opacity 0.2s ease-in-out 0s;
    width: 100%;
    display: block;
    color: #999;
    font-weight: 600;
    opacity: 1;
    padding: 0 0.75em;
    box-sizing: border-box
}}

.feedback-btn__send:hover,
.feedback-btn__send:focus {{
    color: #666;
    text-decoration: none
}}

.feedback-btn--clicked .feedback-btn__send {{
    opacity: 0
}}

.feedback-btn__icon-wrap {{
    position: absolute;
    top: 0;
    left: 0;
    width: 100%
}}

.feedback-btn__icon {{
    -moz-transition: opacity 0.2s, color 0.15s;
    -o-transition: opacity 0.2s, color 0.15s;
    -webkit-transition: opacity 0.2s, color 0.15s;
    transition: opacity 0.2s, color 0.15s;
    -moz-transition-delay: 0s, 0s;
    -o-transition-delay: 0s, 0s;
    -webkit-transition-delay: 0s, 0s;
    transition-delay: 0s, 0s;
    font-size: 22px;
    color: #666;
    opacity: 0;
    width: 50%;
    display: inline-block;
    box-sizing: border-box
}}

.feedback-btn__icon:hover,
.feedback-btn__icon:focus {{
    color: #333
}}

.feedback-btn__icon:first-child {{
    padding-right: 0.5em;
    text-align: right
}}

.feedback-btn__icon:last-child {{
    padding-left: 0.5em;
    text-align: left
}}

.feedback-btn--clicked .feedback-btn__icon {{
    -moz-transition-delay: 0.1s, 0s;
    -o-transition-delay: 0.1s, 0s;
    -webkit-transition-delay: 0.1s, 0s;
    transition-delay: 0.1s, 0s;
    opacity: 1;
    visibility: visible
}}

.feedback-btn__icon--love:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    content: "\29"
}}

.feedback-btn__icon--nolove:before {{
    font-family: 'ddg-serp-icons' !important;
    speak: none;
    font-style: normal;
    font-weight: normal !important;
    font-variant: normal;
    text-transform: none;
    text-decoration: none !important;
    -webkit-font-smoothing: antialiased;
    -moz-osx-font-smoothing: grayscale;
    content: "\28"
}}

.modal__box--feedback {{
    -moz-transition: height 0.3s ease-in-out 0s;
    -o-transition: height 0.3s ease-in-out 0s;
    -webkit-transition: height 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: height 0.3s ease-in-out 0s;
    width: 360px;
    padding: 20px;
    overflow: visible;
    height: auto;
    box-sizing: border-box
}}

.modal__wrap--feedback {{
    height: 100%
}}

.frm--feedback {{
    padding-top: 20px
}}

.feedback-modal__heading {{
    text-align: left
}}

.feedback-modal__heading__prompt {{
    font-size: 0.8em;
    display: block;
    color: #666
}}

.feedback-modal__heading__query {{
    font-weight: 600
}}

.feedback-modal__heading__query:before {{
    content: open-quote
}}

.feedback-modal__heading__query:after {{
    content: close-quote
}}

.feedback-modal__heading--success {{
    text-align: center;
    font-weight: 600
}}

.feedback-modal__label {{
    display: block;
    text-align: left;
    font-weight: normal;
    font-size: 1.1em;
    margin-bottom: 10px
}}

.feedback-modal__input {{
    display: block;
    width: 100%;
    margin-bottom: 10px;
    padding-left: 0.5em;
    height: 2.2em
}}

.feedback-modal__input--dropdown {{
    padding-left: 0
}}

.feedback-modal__input--dropdown select {{
    width: 120% !important;
    height: 100% !important
}}

.feedback-modal__input--text {{
    height: auto;
    padding: 0.5em 0.5em 0.5em 0.75em;
    overflow: auto
}}

.feedback-modal__input--other {{
    -moz-transition: border-width 0.3s, opacity 0.3s, height 0.3s, margin 0.3s;
    -o-transition: border-width 0.3s, opacity 0.3s, height 0.3s, margin 0.3s;
    -webkit-transition: border-width 0.3s, opacity 0.3s, height 0.3s, margin 0.3s;
    transition: border-width 0.3s, opacity 0.3s, height 0.3s, margin 0.3s;
    opacity: 1;
    padding-left: 0.75em
}}

.feedback-modal__input--other.frm__input {{
    background-color: #f7f7f7;
    color: #333
}}

.feedback-modal__input--other.is-invisible {{
    height: 0;
    opacity: 0;
    margin: 0;
    padding: 0;
    border-width: 0
}}

.feedback-modal__submit {{
    width: 100%;
    box-sizing: border-box;
    text-align: center;
    font-size: 1.1em;
    position: relative
}}

.feedback-modal__submit.is-disabled:hover,
.feedback-modal__submit.is-disabled:active,
.feedback-modal__submit.is-disabled:focus {{
    background-color: #c3c3c3;
    border-color: #c3c3c3;
    color: #eee
}}

.feedback-modal__message {{
    font-size: 1.1em;
    text-align: left;
    color: #666
}}

.lt-ie10 .modal__box--feedback {{
    display: block
}}

.results,
.results--didyas,
.results--ads {{
    margin-bottom: 2em;
    margin-top: 0.5em
}}

@media only screen and (max-width: 590px) {{
    .results,
    .results--didyas,
    .results--ads {{
        margin-top: 0
    }}
}}

.results:after,
.results--didyas:after,
.results--ads:after {{
    content: "";
    display: table;
    clear: both
}}

.results {{
    position: relative;
    padding-bottom: 5em
}}

.results-wrapper {{
    padding-top: 0.5em;
    position: relative
}}

.results__sidebar {{
    display: block;
    position: absolute;
    margin-top: 0.8em;
    text-align: left;
    top: 0px;
    left: 735px
}}

@media only screen and (max-width: 1079px) {{
    .results__sidebar {{
        left: 703px
    }}
}}

.results--sidebar,
.results--gutter {{
    position: absolute;
    top: 0.5em
}}

@media only screen and (max-width: 1400px) {{
    .has-module .results--sidebar {{
        display: none
    }}
}}

.results--sidebar--alt {{
    position: fixed;
    bottom: 0;
    background-color: rgba(255, 255, 255, 0.85)
}}

@media only screen and (min-width: 1440px) {{
    .has-module .results--sidebar--alt {{
        padding: 0.5em
    }}
}}

.results--sidebar,
.results--sidebar--alt {{
    right: 0;
    text-align: right;
    z-index: 5
}}

.results--sidebar--mid {{
    -moz-transition: opacity 0.3s ease-in-out 0s;
    -o-transition: opacity 0.3s ease-in-out 0s;
    -webkit-transition: opacity 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: opacity 0.3s ease-in-out 0s;
    opacity: 0;
    visibility: hidden;
    position: fixed;
    width: 100%;
    max-width: 590px;
    left: 0;
    top: 0;
    z-index: 5
}}

.has-active-zci .results--sidebar--mid.can-show {{
    visibility: hidden;
    opacity: 0
}}

.results--sidebar--mid.can-show,
.at-zci-bottom.has-active-zci .results--sidebar--mid {{
    visibility: visible;
    opacity: 1
}}

.results--gutter {{
    text-align: center;
    z-index: 5;
    margin-top: 1em;
    position: fixed
}}

@media only screen and (max-width: 590px) {{
    .results--gutter {{
        text-align: right;
        right: 0.5em;
        left: auto
    }}
}}

.results--powered {{
    padding: 0.5em 1em;
    color: #888
}}

.has-module .results--powered {{
    padding: 0.25em 0.5em;
    opacity: 1
}}

@media only screen and (min-width: 1440px) {{
    .has-module .results--powered {{
        padding: 0.5em
    }}
}}

.results--powered a,
.results--powered a:visited {{
    color: inherit;
    font-size: 0.9em
}}

.results--powered__badge-link:hover {{
    text-decoration: none
}}

.results--powered__badge {{
    vertical-align: baseline
}}

.results--powered__badge.badge--blekko {{
    margin-left: 2px;
    bottom: -1px
}}

.results--powered__badge.badge--bing {{
    margin-left: 3px;
    bottom: -5px
}}

.results--powered__badge.badge--yandex {{
    bottom: -3px
}}

.results--powered__badge.badge--yahoo {{
    bottom: -1px
}}

.no-results {{
    font-size: 1.1667em;
    padding-top: 2em;
    padding-left: 10px;
    padding-bottom: 7px
}}

.no-results.has-related {{
    padding-bottom: 15px
}}

.no-results__related-search {{
    padding-top: 2px
}}

.btn--top {{
    -moz-transition: all 0.3s ease-in-out 0s;
    -o-transition: all 0.3s ease-in-out 0s;
    -webkit-transition: all 0.3s ease-in-out;
    -webkit-transition-delay: 0s;
    transition: all 0.3s ease-in-out 0s;
    line-height: 32px;
    background-color: #dfdfdf;
    background-color: rgba(223, 223, 223, 0.9);
    font-size: 32px;
    color: #666;
    text-indent: 0
}}

.set-header--fixed .btn--top {{
    visibility: hidden
}}

.set-header--fixed.at-zci-bottom .btn--top {{
    visibility: visible
}}

.results--sidebar--mid .btn--top {{
    position: absolute;
    right: -64px;
    top: 24px
}}

.is-module .results--sidebar--mid .btn--top {{
    right: -36px
}}

@media only screen and (max-width: 864px) {{
    .results--sidebar--mid .btn--top {{
        right: -32px
    }}
}}

@media only screen and (max-width: 590px) {{
    .results--sidebar--mid .btn--top {{
        right: 8px
    }}
}}

.is-mobile .btn--top {{
    background-color: rgba(198, 198, 198, 0.8)
}}

.results--didyas {{
    margin-bottom: 0.5em;
    display: none
}}

.results--ads {{
    display: none;
    margin-bottom: 0
}}

.results--ads.has-ad {{
    display: block;
    min-height: 5.65em
}}

@media only screen and (max-width: 590px) {{
    .results--ads.has-ad {{
        min-height: 6.75em
    }}
}}

@media only screen and (max-width: 425px) {{
    .results--ads.has-ad {{
        min-height: 6.75em
    }}
}}

.results .results--ads {{
    margin-top: 1em;
    margin-bottom: 1em
}}

.results--ads--rrail {{
    text-align: left;
    width: 250px;
    margin-top: 1px
}}

.results--ads--rrail .result__body {{
    padding-left: 0;
    padding-right: 0
}}

.answerbar-ads.has-ad--top {{
    border-top: solid 1px #dbdbdb
}}

.answerbar-ads.has-ad--x2 .result--ad .result__snippet {{
    max-width: none;
    min-height: 0
}}

.zci--web.is-active {{
    display: none
}}

.tx--bold {{
    font-weight: 600
}}

.tx--light {{
    font-weight: 300
}}

.tx--italic {{
    font-weight: normal;
    font-style: italic
}}

.tx--25 {{
    font-size: 25px;
    font-size: 1.73611rem
}}

.tx--21 {{
    font-size: 21px;
    font-size: 1.45833rem
}}

.tx--19 {{
    font-size: 19px;
    font-size: 1.31944rem
}}

.tx--17 {{
    font-size: 17px;
    font-size: 1.18056rem
}}

.tx--16 {{
    font-size: 16px;
    font-size: 1.11111rem
}}

.tx--15 {{
    font-size: 15px;
    font-size: 1.04167rem
}}

.tx--14 {{
    font-size: 14px;
    font-size: 0.97222rem
}}

.tx--13 {{
    font-size: 13px;
    font-size: 0.90278rem
}}

.tx--12 {{
    font-size: 12px;
    font-size: 0.83333rem
}}

.tx--11 {{
    font-size: 11px;
    font-size: 0.76389rem
}}

.tx-clr--white {{
    color: #fff
}}

.text--primary,
.tx-clr--dk,
.tx-clr--slate {{
    color: #333
}}

.tx-clr--dk2,
.tx-clr--slate-light {{
    color: #666
}}

.text--secondary,
.tx-clr--grey-dark {{
    color: #888
}}

.tx-clr--lt,
.tx-clr--grey {{
    color: #999
}}

.tx-clr--lt2,
.tx-clr--grey-light {{
    color: #aaa
}}

.tx-clr--silver-dark {{
    color: #ededed
}}

.tx-clr--silver {{
    color: #f2f2f2
}}

.tx-clr--silver-light {{
    color: #f7f7f7
}}

.tx-clr--lt3,
.tx-clr--platinum-darker {{
    color: #c3c3c3
}}

.tx-clr--platinum-dark {{
    color: #d0d0d0
}}

.tx-clr--platinum {{
    color: #e0e0e0
}}

.tx-clr--platinum-light {{
    color: #e5e5e5
}}

.tx-clr--red {{
    color: #de5833
}}

.tx-clr--red-light {{
    color: #e37151
}}

.tx-clr--red-dark {{
    color: #bd4b2b
}}

.tx-clr--blue {{
    color: #4495d4
}}

.tx-clr--blue-light {{
    color: #60a5da
}}

.tx-clr--blue-dark {{
    color: #3a7fb4
}}

.tx-clr--purple {{
    color: #6d59a3
}}

.tx-clr--gold {{
    color: #f1a031
}}

.tx-clr--green {{
    color: #5b9e4d
}}

.bg-tile,
.bg-clr--white {{
    background-color: #fff
}}

.bg-btn {{
    background-color: #fafafa
}}

.bg-clr--dk,
.bg-clr--slate {{
    background-color: #333
}}

.bg-clr--dk2,
.bg-clr--slate-light {{
    background-color: #666
}}

.bg-clr--grey-dark {{
    background-color: #888
}}

.bg-clr--lt,
.bg-clr--grey {{
    background-color: #999
}}

.bg-clr--lt2,
.bg-clr--grey-light {{
    background-color: #aaa
}}

.bg-clr--silver-dark {{
    background-color: #ededed
}}

.bg-clr--silver {{
    background-color: #f2f2f2
}}

.bg-clr--silver-light {{
    background-color: #f7f7f7
}}

.bg-clr--lt3,
.bg-clr--platinum-darker {{
    background-color: #c3c3c3
}}

.bg-clr--platinum-dark {{
    background-color: #d0d0d0
}}

.bg-clr--platinum {{
    background-color: #e0e0e0
}}

.bg-clr--platinum-light {{
    background-color: #e5e5e5
}}

.bg-clr--red {{
    background-color: #de5833
}}

.bg-clr--red-light {{
    background-color: #e37151
}}

.bg-clr--red-dark {{
    background-color: #bd4b2b
}}

.bg-clr--blue {{
    background-color: #4495d4
}}

.bg-clr--blue-light {{
    background-color: #60a5da
}}

.bg-clr--blue-dark {{
    background-color: #3a7fb4
}}

.bg-clr--purple {{
    background-color: #6d59a3
}}

.bg-clr--gold {{
    background-color: #f1a031
}}

.bg-clr--green {{
    background-color: #5b9e4d
}}

.mg--none {{
    margin: 0
}}

.mg--big {{
    margin: 1.25em
}}

.mg--base {{
    margin: 1em
}}

.mg--small {{
    margin: 0.8em
}}

.mg--half {{
    margin: 0.5em
}}

.pd--none {{
    padding: 0
}}

.pd--big {{
    padding: 1.25em
}}

.pd--base {{
    padding: 1em
}}

.pd--small {{
    padding: 0.8em
}}

.pd--half {{
    padding: 0.5em
}}

.set-wide .zcm-wrap--header,
.set-wide .c-info,
.set-wide .c-base,
.set-wide .c-icon,
.set-wide .c-list,
.set-wide .c-product,
.set-wide .c-detail,
.set-wide .zci__main.has-aux,
.set-wide .zci__main--answer,
.set-wide .results,
.set-wide .results--didyas,
.set-wide .results--ads,
.set-wide .results--sidebar--mid {{
    max-width: 864px
}}

.set-wide .cw {{
    max-width: 1282px
}}

.set-wide .header--aside__msg {{
    display: none
}}

.set-wide .header__search-wrap {{
    max-width: 590px
}}

.set-super-wide .zcm-wrap--header,
.set-super-wide .c-info,
.set-super-wide .c-base,
.set-super-wide .c-icon,
.set-super-wide .c-list,
.set-super-wide .c-product,
.set-super-wide .c-detail,
.set-super-wide .zci__main.has-aux,
.set-super-wide .zci__main--answer,
.set-super-wide .results,
.set-super-wide .results--didyas,
.set-super-wide .results--ads,
.set-super-wide .results--sidebar--mid {{
    max-width: 1152px
}}

.set-super-wide .cw {{
    max-width: 1483px
}}

.set-super-wide .header--aside__msg {{
    display: none
}}

.set-super-wide .header__search-wrap {{
    max-width: 590px
}}

.set-underlined-links .result__a {{
    text-decoration: underline
}}

.modules {{
    width: 490px;
    margin-left: 0;
    font-size: initial;
    display: block
}}

@media only screen and (max-width: 864px) {{
    .modules {{
        display: none
    }}
}}

@media only screen and (min-width: 1079px) {{
    .modules {{
        margin-left: 70px
    }}
}}

.results-wrapper--modules {{
    padding-left: 0
}}

@media only screen and (min-width: 1079px) {{
    .results-wrapper--modules {{
        padding-left: 94px
    }}
}}

.modules__about {{
    padding: 22px 22px 18px 22px;
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
    border: 1px solid rgba(150, 150, 150, 0.3);
    border-bottom-color: rgba(125, 125, 125, 0.3);
    font-weight: 400;
    margin-top: 15px;
    margin-bottom: 16px
}}

.modules__about__official,
.js-show-more,
.js-show-more-rows {{
    font-size: 0.875em;
    color: #4495d4
}}

.js-show-more-rows .l {{
    display: none
}}

.modules__about__title {{
    font-size: 1.5em;
    color: #333;
    font-weight: 600;
    line-height: 1em
}}

.modules__about__body {{
    display: table-cell;
    vertical-align: top
}}

.modules__about__body img {{
    padding-left: 25px;
    padding-bottom: 25px
}}

.modules__about__text {{
    font-size: 0.875em;
    line-height: 1.45em;
    color: #666;
    padding-top: 10px
}}

.modules__about__text--chomp {{
    max-height: 7.75em;
    overflow: hidden
}}

.modules__info__rows {{
    color: #666;
    display: table;
    border-spacing: 0 10px
}}

.modules__info__row {{
    display: table-row
}}

.info__row__label {{
    font-weight: 600;
    padding-right: 5px;
    white-space: nowrap
}}

.info__row__label,
.info__row__content {{
    display: table-cell
}}

.modules__info .tile--info,
.modules__info .zci__aux {{
    position: relative;
    width: auto;
    top: 0;
    bottom: 0;
    right: 0
}}

.modules__info .tile--info {{
    font-size: initial;
    font-size: 0.875em;
    padding: 10px 0;
    border-width: 0;
    line-height: inherit
}}

.modules__info .one-line {{
    padding-top: 5px;
    padding-bottom: 5px
}}

.modules__info .info__label {{
    color: inherit;
    font-weight: 600
}}

.modules__info .info__label:after {{
    content: ":"
}}

.modules__info .info__value {{
    text-align: left;
    overflow: visible;
    color: #666
}}

.modules__info .tile--info .info {{
    border-top: 1px solid rgba(150, 150, 150, 0.2);
    line-height: 1.45;
    padding: 5px 0
}}

.modules__info .tile--info .info:first-child {{
    border-top-color: transparent
}}

.modules__about__footer .c-info__link {{
    font-size: 0.875em !important;
    color: #888
}}

.modules__about__footer .zci__more-at__icon {{
    display: none
}}

.modules__news {{
    width: 491px;
    height: 268px
}}

.modules__images,
.modules__news {{
    margin-bottom: 16px
}}

.modules__images a,
.modules__news a {{
    font-size: 0.875em
}}

.modules__images img,
.modules__news img {{
    max-width: none
}}

.modules__images__title,
.modules__news__title {{
    font-weight: bold;
    color: #535353;
    font-size: 0.875em
}}

.modules__images__more,
.modules__news__more {{
    margin-right: 4px
}}

.modules__images__thumbnails,
.modules__news__thumbnails {{
    padding-top: 8px
}}

.modules__news__item {{
    display: inline-block;
    width: 236px;
    height: 238px;
    color: #666;
    border: 1px solid rgba(150, 150, 150, 0.3);
    border-bottom-color: rgba(125, 125, 125, 0.3);
    box-shadow: 0 1px 3px rgba(0, 0, 0, 0.06);
    overflow: hidden;
    cursor: pointer
}}

.modules__news__item:hover {{
    border: 1px solid #333
}}

.modules__news__image {{
    background-color: #f7f7f7;
    height: 120px;
    position: relative
}}

.modules__news__placeholder {{
    position: absolute;
    bottom: 0;
    right: 0;
    left: 0;
    top: 0;
    height: 48px;
    width: 48px;
    margin: auto;
    -moz-border-radius: 50%;
    -webkit-border-radius: 50%;
    border-radius: 50%;
    background-color: #e0e0e0;
    color: #ffffff;
    font-size: 3em;
    padding: 0.5em;
    line-height: 1
}}

.modules__news__item:first-child {{
    margin-right: 14px
}}

.modules__news__item__title {{
    color: #333;
    font-weight: 600;
    font-size: 1em;
    max-height: 62px;
    overflow: hidden;
    line-height: 1.35
}}

.modules__news__item__content {{
    font-size: 0.812em;
    max-height: 106px;
    overflow: hidden;
    margin-top: 15px;
    line-height: 1.4
}}

.modules__news__body {{
    height: 178px;
    padding: 14px
}}

.modules__news__body--img {{
    height: 58px
}}

.modules__news__footer {{
    padding: 0 14px 14px 14px;
    font-size: 0.75em;
    color: #888;
    text-overflow: ellipsis;
    overflow: hidden;
    white-space: nowrap
}}

.modules__news__item__img--bg {{
    background-size: cover;
    width: 100%;
    height: 100%;
    position: relative
}}

.chomp--link__icn--less:before {{
    content: "\229d"
}}
pre code {{
    padding-left: 0;
}}
pre code {{
    background: none;
    white-space: pre;
    overflow: hidden;
}}
pre, tt, code {{
    color: #393939;
    background-color: #eaeaea;
    text-shadow: none;
}}
pre, tt, code {{
    font-family: Consolas, Menlo, Monaco, monospace;
    # color: #fafafa;
    # background: #333;
    -moz-border-radius: 0.25em;
    -webkit-border-radius: 0.25em;
    border-radius: 0.25em;
    background-clip: padding-box;
}}
@font-face {{
    {{
        font-family: 'DDG_ProximaNova';
        src: url("font/ProximaNova-Sbold-webfont.eot");
        src: url("font/ProximaNova-Sbold-webfont.eot?#iefix") format("embedded-opentype"), url("font/ProximaNova-Sbold-webfont.woff") format("woff"), url("font/ProximaNova-Sbold-webfont.ttf") format("truetype"), url("font/ProximaNova-Sbold-webfont.svg#proxima_nova_ltsemibold") format("svg");
        font-weight: 600;
        font-style: normal
    }}
}}

@font-face {{
    {{
        font-family: 'DDG_ProximaNova';
        src: url("font/ProximaNova-Reg-webfont.eot");
        src: url("font/ProximaNova-Reg-webfont.eot?#iefix") format("embedded-opentype"), url("font/ProximaNova-Reg-webfont.woff") format("woff"), url("font/ProximaNova-Reg-webfont.ttf") format("truetype"), url("font/ProximaNova-Reg-webfont.svg#proxima_nova_rgregular") format("svg");
        font-weight: normal;
        font-style: normal
    }}
}}

@font-face {{
    {{
        font-family: 'DDG_ProximaNova';
        src: url("font/ProximaNova-RegIt-webfont.eot");
        src: url("font/ProximaNova-RegIt-webfont.eot?#iefix") format("embedded-opentype"), url("font/ProximaNova-RegIt-webfont.woff") format("woff"), url("font/ProximaNova-RegIt-webfont.ttf") format("truetype"), url("font/ProximaNova-RegIt-webfont.svg#proxima_novaregular_italic") format("svg");
        font-weight: normal;
        font-style: italic
    }}
}}

@font-face {{
    {{
        font-family: 'DDG_ProximaNova';
        src: url("font/ProximaNova-Light-webfont.eot");
        src: url("font/ProximaNova-Light-webfont.eot?#iefix") format("embedded-opentype"), url("font/ProximaNova-Light-webfont.woff") format("woff"), url("font/ProximaNova-Light-webfont.ttf") format("truetype"), url("font/ProximaNova-Light-webfont.svg#proxima_nova_ltlight") format("svg");
        font-weight: 300;
        font-style: normal
    }}
}}

html,
body,
div,
span,
applet,
object,
iframe,
h1,
h2,
h3,
h4,
h5,
h6,
p,
blockquote,
pre,
a,
abbr,
acronym,
address,
big,
cite,
code,
del,
dfn,
em,
img,
ins,
kbd,
q,
s,
samp,
small,
strike,
strong,
sub,
sup,
tt,
var,
b,
u,
i,
center,
dl,
dt,
dd,
ol,
ul,
li,
fieldset,
form,
label,
legend,
table,
caption,
tbody,
tfoot,
thead,
tr,
th,
td,
article,
aside,
canvas,
details,
embed,
figure,
figcaption,
footer,
header,
hgroup,
menu,
nav,
output,
ruby,
section,
summary,
time,
mark,
audio,
video {{
    {{
        margin: 0;
        padding: 0;
        border: 0;
        font: inherit;
        font-size: 100%;
        vertical-align: baseline
    }}
}}

html {{
    {{
        line-height: 1
    }}
}}

ol,
ul {{
    {{
        list-style: none
    }}
}}

table {{
    {{
        border-collapse: collapse;
        border-spacing: 0
    }}
}}

caption,
th,
td {{
    {{
        text-align: left;
        font-weight: normal;
        vertical-align: middle
    }}
}}

q,
blockquote {{
    {{
        quotes: none
    }}
}}

q:before,
q:after,
blockquote:before,
blockquote:after {{
    {{
        content: "";
        content: none
    }}
}}

a img {{
    {{
        border: none
    }}
}}

article,
aside,
details,
figcaption,
figure,
footer,
header,
hgroup,
main,
menu,
nav,
section,
summary {{
    {{
        display: block
    }}
}}

.msg--box,
.msg--info,
.msg--help,
.msg--untranslated,
.msg--warning,
#error_homepage,
.msg,
.content__text,
.header__search-wrap {{
    {{
        max-width: 590px
    }}
}}

.header__logo-wrap:after,
.acp-wrap:after,
.cw:after,
.cw--c:after {{
    {{
        content: "";
        display: block;
        clear: both
    }}
}}

.header__search-wrap,
.content__internal {{
    {{
        padding-left: 94px
    }}
}}

.header__search-wrap,
.content__internal {{
    {{
        padding-right: 94px
    }}
}}

.header__logo-wrap {{
    {{
        display: block;
        width: 94px;
        position: absolute;
        top: 0
    }}
}}

.header__logo-wrap {{
    {{
        left: 0
    }}
}}

@media only screen and (max-width: 1079px) {{
    {{
        .header__search-wrap,
        .content__internal {{
            {{
                padding-left: 58px
            }}
        }}
        .header__logo-wrap {{
            {{
                width: 58px;
                left: 0
            }}
        }}
        .header__search-wrap,
        .content__internal {{
            {{
                padding-right: 58px
            }}
        }}
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .header__search-wrap,
        .content__internal {{
            {{
                padding-left: 58px
            }}
        }}
        .header__logo-wrap {{
            {{
                width: 58px;
                left: 0
            }}
        }}
        .header__search-wrap,
        .content__internal {{
            {{
                padding-right: 58px
            }}
        }}
        .content__internal {{
            {{
                padding-left: 0 !important
            }}
        }}
        .content__internal {{
            {{
                padding-right: 0 !important
            }}
        }}
    }}
}}

@media only screen and (max-width: 425px) {{
    {{
        .header__search-wrap,
        .content__internal {{
            {{
                padding-left: 58px
            }}
        }}
        .header__logo-wrap {{
            {{
                width: 58px;
                left: 0
            }}
        }}
        .header__search-wrap,
        .content__internal {{
            {{
                padding-right: 58px
            }}
        }}
    }}
}}

.logo_homepage__tt {{
    {{
        -moz-transition: opacity 0.3s ease-in-out 0s;
        -o-transition: opacity 0.3s ease-in-out 0s;
        -webkit-transition: opacity 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: opacity 0.3s ease-in-out 0s;
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        visibility: hidden;
        opacity: 0;
        display: inline-block;
        vertical-align: middle;
        position: absolute;
        margin: auto;
        background-color: #a3a3a3;
        background-color: rgba(138, 138, 138, 0.9);
        text-indent: 0px;
        padding: 0 1em;
        white-space: nowrap;
        line-height: 1.6;
        height: 1.6em;
        font-weight: 400;
        font-style: normal;
        color: white;
        z-index: 200
    }}
}}

.logo_homepage__tt {{
    {{
        top: 0;
        bottom: 0;
        left: 100%;
        height: 1.6em
    }}
}}

.logo_homepage__tt:before {{
    {{
        content: "";
        display: block;
        position: absolute;
        margin-top: -4px;
        left: -4px;
        top: 50%;
        border: 4px solid transparent;
        border-left-width: 0;
        border-right-color: #a3a3a3;
        border-right-color: rgba(138, 138, 138, 0.9)
    }}
}}

.rotate {{
    {{
        -webkit-transform: rotate(90deg);
        -moz-transform: rotate(90deg);
        -ms-transform: rotate(90deg);
        -o-transform: rotate(90deg);
        transform: rotate(90deg)
    }}
}}

.ie8 .rotate,
.lt-ie9 .rotate {{
    {{
        filter: progid: DXImageTransform.Microsoft.BasicImage(rotation=1)
    }}
}}

.rotate--45 {{
    {{
        -ms-transform: rotate(45deg);
        -moz-transform: rotate(45deg);
        -o-transform: rotate(45deg);
        -webkit-transform: rotate(45deg);
        transform: rotate(45deg)
    }}
}}

.ie8 .rotate--45,
.lt-ie9 .rotate--45 {{
    {{
        -ms-filter: "progid:DXImageTransform.Microsoft.Matrix(SizingMethod='auto expand', M11=0.7071067811865476, M12=-0.7071067811865475, M21=0.7071067811865475, M22=0.7071067811865476)"
    }}
}}

.rotate--ccw,
.rotate--270 {{
    {{
        -webkit-transform: rotate(-90deg);
        -moz-transform: rotate(-90deg);
        -ms-transform: rotate(-90deg);
        -o-transform: rotate(-90deg);
        transform: rotate(-90deg)
    }}
}}

.ie8 .rotate--ccw,
.lt-ie9 .rotate--ccw,
.ie8 .rotate--270,
.lt-ie9 .rotate--270 {{
    {{
        filter: progid: DXImageTransform.Microsoft.BasicImage(rotation=3)
    }}
}}

.rotate--180 {{
    {{
        -webkit-transform: rotate(180deg);
        -moz-transform: rotate(180deg);
        -ms-transform: rotate(180deg);
        -o-transform: rotate(180deg);
        transform: rotate(180deg)
    }}
}}

.ie8 .rotate--180,
.lt-ie9 .rotate--180 {{
    {{
        filter: progid: DXImageTransform.Microsoft.BasicImage(rotation=2)
    }}
}}

.mg {{
    {{
        margin-top: 1em;
        margin-bottom: 1em
    }}
}}

.mg--half {{
    {{
        margin-top: 0.5em;
        margin-bottom: 0.5em
    }}
}}

.mg--big {{
    {{
        margin-top: 1.25em;
        margin-bottom: 1.25em
    }}
}}

.mg--double {{
    {{
        margin-top: 2em;
        margin-bottom: 2em
    }}
}}

.mg--small {{
    {{
        margin-top: 0.8em;
        margin-bottom: 0.8em
    }}
}}

.mg--quarter {{
    {{
        margin-top: 0.25em;
        margin-bottom: 0.25em
    }}
}}

.mg--none {{
    {{
        margin-top: 0;
        margin-bottom: 0
    }}
}}

.mg-top {{
    {{
        margin-top: 1em
    }}
}}

.mg-top--half {{
    {{
        margin-top: 0.5em
    }}
}}

.mg-top--big {{
    {{
        margin-top: 1.25em
    }}
}}

.mg-top--double {{
    {{
        margin-top: 2em
    }}
}}

.mg-top--small {{
    {{
        margin-top: 0.8em
    }}
}}

.mg-top--quarter {{
    {{
        margin-top: 0.25em
    }}
}}

.mg-top--none {{
    {{
        margin-top: 0
    }}
}}

.mg-bottom {{
    {{
        margin-bottom: 1em
    }}
}}

.mg-bottom--half {{
    {{
        margin-bottom: 0.5em
    }}
}}

.mg-bottom--big {{
    {{
        margin-bottom: 1.25em
    }}
}}

.mg-bottom--double {{
    {{
        margin-bottom: 2em
    }}
}}

.mg-bottom--small {{
    {{
        margin-bottom: 0.8em
    }}
}}

.mg-bottom--quarter {{
    {{
        margin-bottom: 0.25em
    }}
}}

.mg-bottom--none {{
    {{
        margin-bottom: 0
    }}
}}

.pd {{
    {{
        padding-top: 1em;
        padding-bottom: 1em
    }}
}}

.pd--big {{
    {{
        padding-top: 1.25em;
        padding-bottom: 1.25em
    }}
}}

.pd--double {{
    {{
        padding-top: 2em;
        padding-bottom: 2em
    }}
}}

.pd--small {{
    {{
        padding-top: 0.8em;
        padding-bottom: 0.8em
    }}
}}

.pd--quarter {{
    {{
        padding-top: 0.25em;
        padding-bottom: 0.25em
    }}
}}

.pd-top {{
    {{
        padding-top: 1em
    }}
}}

.pd-top--half {{
    {{
        padding-top: 0.5em
    }}
}}

.pd-top--big {{
    {{
        padding-top: 1.25em
    }}
}}

.pd-top--double {{
    {{
        padding-top: 2em
    }}
}}

.pd-top--small {{
    {{
        padding-top: 0.8em
    }}
}}

.pd-top--quarter {{
    {{
        padding-top: 0.25em
    }}
}}

.pd-top--none {{
    {{
        padding-top: 0
    }}
}}

.pd-bottom {{
    {{
        padding-bottom: 1em
    }}
}}

.pd-bottom--half {{
    {{
        padding-bottom: 0.5em
    }}
}}

.pd-bottom--big {{
    {{
        padding-bottom: 1.25em
    }}
}}

.pd-bottom--double {{
    {{
        padding-bottom: 2em
    }}
}}

.pd-bottom--small {{
    {{
        padding-bottom: 0.8em
    }}
}}

.pd-bottom--quarter {{
    {{
        padding-bottom: 0.25em
    }}
}}

.pd-bottom--none {{
    {{
        padding-bottom: 0
    }}
}}

h1,
h2,
h3,
h4,
h5,
h6,
p,
ul,
ol,
blockquote {{
    {{
        padding-top: 0.5em;
        padding-bottom: 0.5em
    }}
}}

h1,
.h-xxl,
.hd-lg,
.t-xxxxl,
.t-triple {{
    {{
        font-size: 3em
    }}
}}

h2,
.h-xl,
.t-xxxl,
.t-double {{
    {{
        font-size: 2em
    }}
}}

h3,
.h-l,
.t-xxl {{
    {{
        font-size: 1.75em
    }}
}}

h4,
.h-m,
.hd-md,
.t-xl {{
    {{
        font-size: 1.5em
    }}
}}

h5,
.h-s,
.t-l {{
    {{
        font-size: 1.33em
    }}
}}

h6,
.h-xs,
.t-m {{
    {{
        font-size: 1.1667em
    }}
}}

small,
.t-s,
.acp--bang__body {{
    {{
        font-size: 0.9176em
    }}
}}

.t-xs {{
    {{
        font-size: 0.8333em
    }}
}}

.t-xxs {{
    {{
        font-size: 0.75em
    }}
}}

.t-xxxs {{
    {{
        font-size: 0.66em
    }}
}}

.t-xxxxs {{
    {{
        font-size: 0.5833em
    }}
}}

.t-xxxxxs,
.t-half {{
    {{
        font-size: 0.5em
    }}
}}

.t-n {{
    {{
        font-size: 1em
    }}
}}

.t-nat {{
    {{
        font-size: 1rem
    }}
}}

.text-center,
.text-mid,
.t-mid,
.t-center {{
    {{
        text-align: center
    }}
}}

.t-left,
.text-left {{
    {{
        text-align: left
    }}
}}

.t-right,
.text-right {{
    {{
        text-align: right
    }}
}}

.t-bold {{
    {{
        font-weight: bold
    }}
}}

.t-normal {{
    {{
        font-weight: normal
    }}
}}

@media only screen and (min-width: 1079px) {{
    {{
        .h-xxl--screen-l,
        .t-xxxxl--screen-l,
        .t-triple--screen-l {{
            {{
                font-size: 3em
            }}
        }}
        .h-xl--screen-l,
        .t-xxxl--screen-l,
        .t-double--screen-l {{
            {{
                font-size: 2em
            }}
        }}
        .h-l--screen-l,
        .t-xxl--screen-l {{
            {{
                font-size: 1.75em
            }}
        }}
        .h-m--screen-l,
        .t-xl--screen-l,
        .t-xl--screen-l {{
            {{
                font-size: 1.5em
            }}
        }}
        .h-s--screen-l,
        .t-l--screen-l {{
            {{
                font-size: 1.33em
            }}
        }}
        .h-xs--screen-l,
        .t-m--screen-l {{
            {{
                font-size: 1.1667em
            }}
        }}
        .t-s--screen-l {{
            {{
                font-size: 0.9176em
            }}
        }}
        .t-xs--screen-l {{
            {{
                font-size: 0.8333em
            }}
        }}
        .t-nat--screen-l {{
            {{
                font-size: 1rem
            }}
        }}
        .t-half--screen-l {{
            {{
                font-size: 0.5em
            }}
        }}
        .t-n--screen-l {{
            {{
                font-size: 1em
            }}
        }}
        .t-mid--screen-l,
        .t-center--screen-l {{
            {{
                text-align: center
            }}
        }}
        .t-left--screen-l {{
            {{
                text-align: left
            }}
        }}
        .t-right--screen-l {{
            {{
                text-align: right
            }}
        }}
    }}
}}

@media only screen and (min-width: 1440px) {{
    {{
        .h-xxl--screen-xl,
        .t-xxxxl--screen-xl,
        .t-triple--screen-xl {{
            {{
                font-size: 3em
            }}
        }}
        .h-xl--screen-xl,
        .t-xxxl--screen-xl,
        .t-double--screen-xl {{
            {{
                font-size: 2em
            }}
        }}
        .h-l--screen-xl,
        .t-xxl--screen-xl {{
            {{
                font-size: 1.75em
            }}
        }}
        .h-m--screen-xl,
        .t-xl--screen-xl,
        .t-xl--screen-xl {{
            {{
                font-size: 1.5em
            }}
        }}
        .h-s--screen-xl,
        .t-l--screen-xl {{
            {{
                font-size: 1.33em
            }}
        }}
        .h-xs--screen-xl,
        .t-m--screen-xl {{
            {{
                font-size: 1.1667em
            }}
        }}
        .t-s--screen-xl {{
            {{
                font-size: 0.9176em
            }}
        }}
        .t-xs--screen-xl {{
            {{
                font-size: 0.8333em
            }}
        }}
        .t-nat--screen-xl {{
            {{
                font-size: 1rem
            }}
        }}
        .t-half--screen-xl {{
            {{
                font-size: 0.5em
            }}
        }}
        .t-n--screen-xl {{
            {{
                font-size: 1em
            }}
        }}
        .t-mid--screen-xl,
        .t-center--screen-xl {{
            {{
                text-align: center
            }}
        }}
        .t-left--screen-xl {{
            {{
                text-align: left
            }}
        }}
        .t-right--screen-xl {{
            {{
                text-align: right
            }}
        }}
    }}
}}

@media only screen and (max-width: 864px) {{
    {{
        .h-xxl--screen-m,
        .t-xxxxl--screen-m,
        .t-triple--screen-m {{
            {{
                font-size: 3em
            }}
        }}
        .h-xl--screen-m,
        .t-xxxl--screen-m,
        .t-double--screen-m {{
            {{
                font-size: 2em
            }}
        }}
        .h-l--screen-m,
        .t-xxl--screen-m {{
            {{
                font-size: 1.75em
            }}
        }}
        .h-m--screen-m,
        .t-xl--screen-m,
        .t-xl--screen-m {{
            {{
                font-size: 1.5em
            }}
        }}
        .h-s--screen-m,
        .t-l--screen-m {{
            {{
                font-size: 1.33em
            }}
        }}
        .h-xs--screen-m,
        .t-m--screen-m {{
            {{
                font-size: 1.1667em
            }}
        }}
        .t-s--screen-m {{
            {{
                font-size: 0.9176em
            }}
        }}
        .t-xs--screen-m {{
            {{
                font-size: 0.8333em
            }}
        }}
        .t-nat--screen-m {{
            {{
                font-size: 1rem
            }}
        }}
        .t-half--screen-m {{
            {{
                font-size: 0.5em
            }}
        }}
        .t-n--screen-m {{
            {{
                font-size: 1em
            }}
        }}
        .t-mid--screen-m,
        .t-center--screen-m {{
            {{
                text-align: center
            }}
        }}
        .t-left--screen-m {{
            {{
                text-align: left
            }}
        }}
        .t-right--screen-m {{
            {{
                text-align: right
            }}
        }}
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .h-xxl--screen-s,
        .t-xxxxl--screen-s,
        .t-triple--screen-s {{
            {{
                font-size: 3em
            }}
        }}
        .h-xl--screen-s,
        .t-xxxl--screen-s,
        .t-double--screen-s {{
            {{
                font-size: 2em
            }}
        }}
        .h-l--screen-s,
        .t-xxl--screen-s {{
            {{
                font-size: 1.75em
            }}
        }}
        .h-m--screen-s,
        .t-xl--screen-s,
        .t-xl--screen-s {{
            {{
                font-size: 1.5em
            }}
        }}
        .h-s--screen-s,
        .t-l--screen-s {{
            {{
                font-size: 1.33em
            }}
        }}
        .h-xs--screen-s,
        .t-m--screen-s {{
            {{
                font-size: 1.1667em
            }}
        }}
        .t-s--screen-s {{
            {{
                font-size: 0.9176em
            }}
        }}
        .t-xs--screen-s {{
            {{
                font-size: 0.8333em
            }}
        }}
        .t-nat--screen-s {{
            {{
                font-size: 1rem
            }}
        }}
        .t-half--screen-s {{
            {{
                font-size: 0.5em
            }}
        }}
        .t-n--screen-s {{
            {{
                font-size: 1em
            }}
        }}
        .t-mid--screen-s,
        .t-center--screen-s {{
            {{
                text-align: center
            }}
        }}
        .t-left--screen-s {{
            {{
                text-align: left
            }}
        }}
        .t-right--screen-s {{
            {{
                text-align: right
            }}
        }}
    }}
}}

@media only screen and (max-width: 425px) {{
    {{
        .h-xxl--screen-xs,
        .t-xxxxl--screen-xs,
        .t-triple--screen-xs {{
            {{
                font-size: 3em
            }}
        }}
        .h-xl--screen-xs,
        .t-xxxl--screen-xs,
        .t-double--screen-xs {{
            {{
                font-size: 2em
            }}
        }}
        .h-l--screen-xs,
        .t-xxl--screen-xs {{
            {{
                font-size: 1.75em
            }}
        }}
        .h-m--screen-xs,
        .t-xl--screen-xs,
        .t-xl--screen-xs {{
            {{
                font-size: 1.5em
            }}
        }}
        .h-s--screen-xs,
        .t-l--screen-xs {{
            {{
                font-size: 1.33em
            }}
        }}
        .h-xs--screen-xs,
        .t-m--screen-xs {{
            {{
                font-size: 1.1667em
            }}
        }}
        .t-s--screen-xs {{
            {{
                font-size: 0.9176em
            }}
        }}
        .t-xs--screen-xs {{
            {{
                font-size: 0.8333em
            }}
        }}
        .t-nat--screen-xs {{
            {{
                font-size: 1rem
            }}
        }}
        .t-half--screen-xs {{
            {{
                font-size: 0.5em
            }}
        }}
        .t-n--screen-xs {{
            {{
                font-size: 1em
            }}
        }}
        .t-mid--screen-xs,
        .t-center--screen-xs {{
            {{
                text-align: center
            }}
        }}
        .t-left--screen-xs {{
            {{
                text-align: left
            }}
        }}
        .t-right--screen-xs {{
            {{
                text-align: right
            }}
        }}
    }}
}}

.button,
.butt,
.btn,
.tag,
.btn,
.button,
.btn--icon,
.btn--top,
.header__button,
.header__button--menu {{
    {{
        display: inline-block;
        vertical-align: middle;
        white-space: nowrap;
        text-align: center;
        position: relative;
        text-decoration: none;
        margin-top: 0;
        margin-bottom: 0;
        padding: 0 1em;
        line-height: 2.5;
        border: 1px solid #babec9;
        text-shadow: 0 1px 1px rgba(255, 255, 255, 0.1);
        background-color: #f8f8f8;
        color: #474747;
        -moz-border-radius: 0.25em;
        -webkit-border-radius: 0.25em;
        border-radius: 0.25em;
        -webkit-box-sizing: content-box;
        -moz-box-sizing: content-box;
        -ms-box-sizing: content-box;
        -o-box-sizing: content-box;
        box-sizing: content-box;
        -webkit-user-select: none;
        -khtml-user-select: none;
        -moz-user-select: -moz-none;
        -ms-user-select: none;
        user-select: none
    }}
}}

.button,
.butt,
.btn,
.btn,
.button,
.btn--icon,
.btn--top,
.header__button,
.header__button--menu {{
    {{
        cursor: pointer
    }}
}}

.button:hover,
.butt:hover,
.btn:hover,
.btn:hover,
.button:hover {{
    {{
        text-decoration: none;
        background-color: #fff
    }}
}}

.button:active,
.butt:active,
.btn:active,
.btn:active,
.button:active {{
    {{
        background-color: #eee
    }}
}}

.button:active,
.button:focus {{
    {{
        outline: none
    }}
}}

.btn--full,
.btn--fill {{
    {{
        display: block
    }}
}}

.btn--pill {{
    {{
        -moz-border-radius: 5em;
        -webkit-border-radius: 5em;
        border-radius: 5em
    }}
}}

.btn--xxl {{
    {{
        font-size: 5em
    }}
}}

.btn--xl {{
    {{
        font-size: 3em
    }}
}}

.btn--l {{
    {{
        font-size: 2em
    }}
}}

.btn--m {{
    {{
        font-size: 1.5em
    }}
}}

.btn--s {{
    {{
        font-size: 0.75em
    }}
}}

.btn--xs {{
    {{
        font-size: 0.5em
    }}
}}

.btn--xtall {{
    {{
        line-height: 4
    }}
}}

.btn--tall {{
    {{
        line-height: 3
    }}
}}

.btn--short {{
    {{
        line-height: 1.5
    }}
}}

.btn--nat {{
    {{
        line-height: inherit
    }}
}}

.btn--dark,
.tag--dark {{
    {{
        border-color: #242424;
        text-shadow: 0 -1px 1px rgba(0, 0, 0, 0.9);
        background-color: #474747;
        color: #f8f8f8
    }}
}}

.btn--dark:hover {{
    {{
        background-color: #383838
    }}
}}

.btn--dark:active {{
    {{
        background-color: #292929
    }}
}}

.btn--grp,
.btn-grp>.btn {{
    {{
        margin-left: -0.35em
    }}
}}

.btn--grp,
.btn-grp>.btn {{
    {{
        -webkit-border-radius: 0;
        -moz-border-radius: 0;
        border-radius: 0
    }}
}}

.btn--grp:first-child,
.btn-grp>.btn:first-child,
.btn--grp--first {{
    {{
        margin-left: 0;
        -moz-border-radius-bottomleft: 0.25em;
        -webkit-border-bottom-left-radius: 0.25em;
        border-bottom-left-radius: 0.25em;
        -moz-border-radius-topleft: 0.25em;
        -webkit-border-top-left-radius: 0.25em;
        border-top-left-radius: 0.25em
    }}
}}

.btn--grp:last-child,
.btn-grp>.btn:last-child,
.btn--grp--last {{
    {{
        -moz-border-radius-topright: 0.25em;
        -webkit-border-top-right-radius: 0.25em;
        border-top-right-radius: 0.25em;
        -moz-border-radius-bottomright: 0.25em;
        -webkit-border-bottom-right-radius: 0.25em;
        border-bottom-right-radius: 0.25em
    }}
}}

.btn--grp.btn--grp--fakefirst {{
    {{
        -webkit-border-radius: 0;
        -moz-border-radius: 0;
        border-radius: 0
    }}
}}

.circle {{
    {{
        -moz-border-radius: 50%;
        -webkit-border-radius: 50%;
        border-radius: 50%
    }}
}}

.round {{
    {{
        -moz-border-radius: 0.25em;
        -webkit-border-radius: 0.25em;
        border-radius: 0.25em
    }}
}}

pre,
tt,
code {{
    {{
        font-family: Consolas, Menlo, Monaco, monospace;
        color: #fafafa;
        background: #333;
        -moz-border-radius: 0.25em;
        -webkit-border-radius: 0.25em;
        border-radius: 0.25em;
        background-clip: padding-box
    }}
}}

tt {{
    {{
        padding: 0 3px 1px
    }}
}}

pre {{
    {{
        margin: 0 0.5em 1em 0;
        overflow: auto;
        padding: 1em;
        -moz-tab-size: 1;
        -o-tab-size: 1;
        tab-size: 1;
        -ms-word-break: normal;
        word-break: normal;
        -webkit-hyphens: none;
        -moz-hyphens: none;
        -ms-hyphens: none;
        hyphens: none;
        position: relative;
        display: block;
        max-width: 100%;
        white-space: pre-wrap;
        white-space: -moz-pre-wrap;
        white-space: -pre-wrap;
        white-space: -o-pre-wrap
    }}
}}

pre code {{
    {{
        background: none;
        white-space: pre;
        overflow: hidden
    }}
}}

.media,
.acp--bang {{
    {{
        display: block
    }}
}}

.media__img {{
    {{
        float: left;
        margin-right: 0.5em
    }}
}}

.media__img--rev {{
    {{
        float: right;
        margin-left: 0.5em
    }}
}}

.media__img img,
.media__img--rev img {{
    {{
        display: block
    }}
}}

.media__body,
.acp--bang__body {{
    {{
        overflow: hidden
    }}
}}

table {{
    {{
        display: table
    }}
}}

html,
body {{
    {{
        height: 100%;
        min-height: 100%;
        padding: 0;
        margin: 0
    }}
}}

html {{
    {{
        overflow: hidden;
        overflow-y: auto;
        background-color: #f7f7f7;
        font-size: 90%;
        -webkit-text-size-adjust: 100%
    }}
}}

body {{
    {{
        overflow: hidden;
        position: relative;
        height: auto
    }}
}}

.site-wrapper {{
    {{
        overflow: hidden;
        min-height: 100%
    }}
}}

.is-mobile-device.has-search-focus .site-wrapper:before {{
    {{
        display: none
    }}
}}

body {{
    {{
        background-color: #fff;
        line-height: 1.6
    }}
}}

body,
input,
select,
textarea {{
    {{
        font-family: "DDG_ProximaNova", "DDG_ProximaNova_UI_0", "DDG_ProximaNova_UI_1", "DDG_ProximaNova_UI_2", "DDG_ProximaNova_UI_3", "DDG_ProximaNova_UI_4", "DDG_ProximaNova_UI_5", "DDG_ProximaNova_UI_6", "Proxima Nova", "Helvetica Neue", "Helvetica", "Segoe UI", "Nimbus Sans L", "Liberation Sans", "Open Sans", FreeSans, Arial, sans-serif;
        color: #333
    }}
}}

h1,
h2,
h3,
h4,
h5,
h6 {{
    {{
        font-weight: normal
    }}
}}

p {{
    {{
        line-height: 1.35;
        padding-top: 0.25em;
        padding-bottom: 0.25em
    }}
}}

strong,
b {{
    {{
        font-weight: 600
    }}
}}

em,
i {{
    {{
        font-style: italic
    }}
}}

pre,
tt,
code {{
    {{
        color: #393939;
        background-color: #eaeaea;
        text-shadow: none
    }}
}}

pre {{
    {{
        padding: 0.55em 0;
        padding-left: 0.5em;
        margin: 0.5em 0
    }}
}}

pre code {{
    {{
        padding-left: 0
    }}
}}

sup,
sub {{
    {{
        vertical-align: baseline;
        font-size: 0.6em;
        position: relative;
        line-height: 0
    }}
}}

sup {{
    {{
        top: -0.5em
    }}
}}

sub {{
    {{
        top: 0
    }}
}}

.no-select,
.nav,
.nav-menu__icon,
.nav-menu__close {{
    {{
        -webkit-user-select: none;
        -khtml-user-select: none;
        -moz-user-select: -moz-none;
        -ms-user-select: none;
        user-select: none
    }}
}}

.absolute-center {{
    {{
        position: absolute;
        margin: auto;
        bottom: 0;
        right: 0;
        left: 0;
        top: 0
    }}
}}

.one-line {{
    {{
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis
    }}
}}

a {{
    {{
        font-family: inherit;
        text-decoration: none;
        color: #4495d4
    }}
}}

a:hover,
a:focus {{
    {{
        color: #3a7fb4
    }}
}}

a:hover {{
    {{
        text-decoration: underline
    }}
}}

a:focus {{
    {{
        text-decoration: none;
        outline: none
    }}
}}

a.is-disabled {{
    {{
        color: #999;
        cursor: default
    }}
}}

a.is-disabled:focus,
a.is-disabled:hover {{
    {{
        text-decoration: none;
        color: #999
    }}
}}

:visited {{
    {{
        color: #6d59a3
    }}
}}

.no-visited {{
    {{
        color: #4495d4
    }}
}}

@media only screen and (max-height: 590px) {{
    {{
        .has-slideout-open {{
            {{
                overflow: hidden
            }}
        }}
    }}
}}

.anchor,
.anchor--inline--first,
.anchor--inline {{
    {{
        position: relative;
        display: inline-block;
        height: 0;
        width: 0;
        margin: 0
    }}
}}

.anchor {{
    {{
        float: left
    }}
}}

.anchor--inline--first,
.anchor--inline {{
    {{
        margin-left: -0.5ex;
        vertical-align: top
    }}
}}

.anchor--inline--first {{
    {{
        margin-left: 0
    }}
}}

.btn,
.button,
.btn--icon,
.btn--top,
.header__button,
.header__button--menu {{
    {{
        -webkit-appearance: none;
        -moz-appearance: none;
        -ms-appearance: none;
        -o-appearance: none;
        appearance: none;
        -moz-transition: none 0.3s ease-in-out 0s;
        -o-transition: none 0.3s ease-in-out 0s;
        -webkit-transition: none 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: none 0.3s ease-in-out 0s;
        outline: none !important;
        background-color: #fafafa;
        border-color: #ddd;
        line-height: 2;
        font-weight: normal;
        font-size: 1em;
        padding-top: 2px;
        padding-bottom: 2px;
        color: #333
    }}
}}

.btn:hover,
.button:hover,
.btn:focus,
.button:focus,
.btn--icon:hover,
.btn--top:hover,
.header__button:hover,
.header__button--menu:hover,
.btn--icon:focus,
.btn--top:focus,
.header__button:focus,
.header__button--menu:focus {{
    {{
        text-decoration: none !important;
        color: #333
    }}
}}

.btn:active,
.button:active {{
    {{
        background-color: #fafafa;
        border-color: #fafafa
    }}
}}

.btn.is-disabled,
.button.is-disabled {{
    {{
        border-color: #c3c3c3;
        background-color: #c3c3c3;
        color: #eee
    }}
}}

.btn.is-active,
.button.is-active {{
    {{
        border-color: transparent;
        background: #666;
        color: #fff;
        font-weight: 300
    }}
}}

.btn--primary,
.is-checked .frm__switch__label {{
    {{
        background-color: #60a5da;
        border-color: #60a5da;
        color: #fff;
        font-weight: 600
    }}
}}

.btn--primary:hover,
.is-checked .frm__switch__label:hover,
.btn--primary:focus,
.is-checked .frm__switch__label:focus {{
    {{
        color: #fff;
        background-color: #4495d4;
        border-color: #4495d4
    }}
}}

.btn--primary:active,
.is-checked .frm__switch__label:active {{
    {{
        background-color: #3a7fb4;
        border-color: #3a7fb4
    }}
}}

.btn--primary--alt {{
    {{
        background-color: #4495d4;
        border-color: #4495d4;
        color: #fff;
        font-weight: 600
    }}
}}

.btn--primary--alt:hover,
.btn--primary--alt:focus {{
    {{
        color: #fff;
        background-color: #3a7fb4;
        border-color: #3a7fb4
    }}
}}

.btn--primary--alt:active {{
    {{
        background-color: #27567a;
        border-color: #27567a
    }}
}}

.btn--critical {{
    {{
        background-color: #e37151;
        border-color: #e37151;
        color: #fff;
        font-weight: 600
    }}
}}

.btn--critical:hover,
.btn--critical:focus {{
    {{
        color: #fff;
        background-color: #de5833;
        border-color: #de5833
    }}
}}

.btn--critical:active {{
    {{
        background-color: #bd4b2b;
        border-color: #bd4b2b
    }}
}}

.btn--secondary {{
    {{
        border-color: #ddd;
        background-color: #f2f2f2;
        color: #666;
        font-weight: 600
    }}
}}

.btn--secondary:hover,
.btn--secondary:focus {{
    {{
        background-color: #60a5da;
        border-color: #4495d4;
        color: #fff
    }}
}}

.btn--secondary:active {{
    {{
        background-color: #4495d4;
        color: #fff
    }}
}}

.btn--alt {{
    {{
        border-color: #c3c3c3;
        background-color: #e5e5e5;
        color: #666
    }}
}}

.btn--alt:hover,
.btn--alt:focus {{
    {{
        color: #666;
        background-color: #d8d8d8
    }}
}}

.btn--alt:active {{
    {{
        background-color: #e5e5e5;
        border-color: #e5e5e5
    }}
}}

.btn--wire {{
    {{
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        background: none;
        background-color: transparent;
        font-weight: normal;
        border: 1px solid #ddd;
        color: #aaa
    }}
}}

.btn--wire:hover,
.btn--wire:focus {{
    {{
        border-color: #aaa;
        background-color: #aaa;
        color: #fff
    }}
}}

.btn--wire--hero {{
    {{
        -moz-border-radius: 6px;
        -webkit-border-radius: 6px;
        border-radius: 6px;
        background: none;
        background-color: transparent;
        border: 2px solid #fff;
        color: #fff
    }}
}}

.btn--wire--hero:hover,
.btn--wire--hero:focus {{
    {{
        border-color: #fff;
        background: none;
        background-color: transparent;
        color: #fff
    }}
}}

.btn--wire--dark {{
    {{
        border-color: #333;
        color: #333
    }}
}}

.btn--wire--dark:hover {{
    {{
        border-color: #333;
        background-color: #333
    }}
}}

.btn--icon,
.btn--top,
.header__button,
.header__button--menu {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        -moz-transition: background-color 0.1s, color 0.1s;
        -o-transition: background-color 0.1s, color 0.1s;
        -webkit-transition: background-color 0.1s, color 0.1s;
        transition: background-color 0.1s, color 0.1s;
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        -webkit-tap-highlight-color: transparent;
        padding: 0;
        border: none;
        background: none;
        text-align: center;
        text-shadow: none;
        vertical-align: middle;
        position: relative;
        width: 33px;
        height: 33px;
        line-height: 34px;
        text-indent: 1px;
        font-size: 16px
    }}
}}

.no-touch .btn--icon:hover,
.no-touch .btn--top:hover,
.no-touch .header__button:hover,
.no-touch .header__button--menu:hover,
.no-touch .btn--icon:focus,
.no-touch .btn--top:focus,
.no-touch .header__button:focus,
.no-touch .header__button--menu:focus,
.no-js .btn--icon:hover,
.no-js .btn--top:hover,
.no-js .header__button:hover,
.no-js .header__button--menu:hover,
.no-js .btn--icon:focus,
.no-js .btn--top:focus,
.no-js .header__button:focus,
.no-js .header__button--menu:focus {{
    {{
        color: white;
        background-color: #6d6d6d
    }}
}}

.no-touch .btn--icon:active,
.no-touch .btn--top:active,
.no-touch .header__button:active,
.no-touch .header__button--menu:active,
.no-js .btn--icon:active,
.no-js .btn--top:active,
.no-js .header__button:active,
.no-js .header__button--menu:active {{
    {{
        -moz-transition: none 0.3s ease-in-out 0s;
        -o-transition: none 0.3s ease-in-out 0s;
        -webkit-transition: none 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: none 0.3s ease-in-out 0s;
        color: white;
        background-color: #555
    }}
}}

.btn__icon,
.btn__icon--sm {{
    {{
        margin-right: 0.125em;
        vertical-align: middle;
        margin-top: -0.125em;
        max-height: 1em;
        max-width: 1em
    }}
}}

.btn__icon {{
    {{
        margin-left: -0.25em;
        font-size: 1.5em
    }}
}}

.btn__icon--sm {{
    {{
        font-size: 16px;
        margin-left: -0.25em;
        margin-right: 0.5em;
        position: relative;
        top: -0.05em
    }}
}}

.btn--inline {{
    {{
        display: inline;
        line-height: inherit;
        padding-left: 0.75em;
        padding-right: 0.75em;
        top: -1px
    }}
}}

.btn-stack {{
    {{
        margin-bottom: -0.5em;
        padding: 0
    }}
}}

.btn-stack .btn {{
    {{
        display: block;
        margin-bottom: 0.5em
    }}
}}

.colorpicker {{
    {{
        -moz-box-shadow: 0 1px 4px rgba(0, 0, 0, 0.2);
        -webkit-box-shadow: 0 1px 4px rgba(0, 0, 0, 0.2);
        box-shadow: 0 1px 4px rgba(0, 0, 0, 0.2);
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        position: absolute;
        display: none;
        top: 0;
        left: 0;
        background: #fff;
        padding: 15px;
        height: 120px;
        width: 145px
    }}
}}

.lt-ie9 .colorpicker {{
    {{
        border: 1px solid #eee
    }}
}}

.colorpicker__nub {{
    {{
        -moz-transform: rotate(45deg);
        -ms-transform: rotate(45deg);
        -webkit-transform: rotate(45deg);
        transform: rotate(45deg);
        position: absolute;
        width: 10px;
        height: 10px;
        top: 50%;
        margin-top: -5px;
        right: -6px;
        background: #fff;
        border-top: 1px solid #eee;
        border-right: 1px solid #eee
    }}
}}

.lt-ie9 .colorpicker__nub {{
    {{
        display: none
    }}
}}

.colorpicker__2d {{
    {{
        -moz-border-radius: 4px;
        -webkit-border-radius: 4px;
        border-radius: 4px;
        position: relative;
        display: block;
        float: left;
        width: 120px;
        height: 120px;
        cursor: pointer
    }}
}}

.colorpicker__2d-white,
.colorpicker__2d-black {{
    {{
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        position: absolute;
        width: 120px;
        height: 120px
    }}
}}

.colorpicker__2d-white {{
    {{
        z-index: 0;
        background: -moz-linear-gradient(left, #fff 0%, rgba(255, 255, 255, 0) 100%);
        background: -webkit-gradient(linear, left top, right top, color-stop(0%, #fff), color-stop(100%, rgba(255, 255, 255, 0)));
        background: -webkit-linear-gradient(left, #fff 0%, rgba(255, 255, 255, 0) 100%);
        background: -o-linear-gradient(left, #fff 0%, rgba(255, 255, 255, 0) 100%);
        background: linear-gradient(to right, #ffffff 0%, rgba(255, 255, 255, 0) 100%);
        -ms-filter: "progid:DXImageTransform.Microsoft.gradient(GradientType = 1, startColorstr=#FFFFFFFF, endColorstr=#00FFFFFF)";
        filter: progid: DXImageTransform.Microsoft.gradient(GradientType=1, startColorstr='#FFFFFFFF', endColorstr='#00FFFFFF')
    }}
}}

.colorpicker__2d-black {{
    {{
        -moz-box-shadow: inset 0 0 2px rgba(0, 0, 0, 0.2);
        -webkit-box-shadow: inset 0 0 2px rgba(0, 0, 0, 0.2);
        box-shadow: inset 0 0 2px rgba(0, 0, 0, 0.2);
        z-index: 1;
        background: -moz-linear-gradient(top, transparent 0%, #000 100%);
        background: -webkit-gradient(linear, left top, left bottom, color-stop(0%, transparent), color-stop(100%, #000));
        background: -webkit-linear-gradient(top, transparent 0%, #000 100%);
        background: -o-linear-gradient(top, transparent 0%, #000 100%);
        background: linear-gradient(to bottom, rgba(0, 0, 0, 0) 0%, #000000 100%);
        -ms-filter: "progid:DXImageTransform.Microsoft.gradient(GradientType = 0, startColorstr=#00000000, endColorstr=#FF000000)";
        filter: progid: DXImageTransform.Microsoft.gradient(GradientType=0, startColorstr='#00000000', endColorstr='#FF000000')
    }}
}}

.colorpicker__2d-marker {{
    {{
        -moz-border-radius: 6px;
        -webkit-border-radius: 6px;
        border-radius: 6px;
        -moz-box-shadow: 0 1px 2px rgba(0, 0, 0, 0.3);
        -webkit-box-shadow: 0 1px 2px rgba(0, 0, 0, 0.3);
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.3);
        background: #fff;
        border: 1px solid #ddd;
        position: absolute;
        margin: -6px 0 0 -6px;
        width: 10px;
        height: 10px;
        z-index: 2
    }}
}}

.colorpicker__1d {{
    {{
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        -moz-box-shadow: inset 0 0 2px rgba(0, 0, 0, 0.2);
        -webkit-box-shadow: inset 0 0 2px rgba(0, 0, 0, 0.2);
        box-shadow: inset 0 0 2px rgba(0, 0, 0, 0.2);
        position: relative;
        display: block;
        margin-left: 10px;
        width: 15px;
        float: left;
        height: 120px;
        cursor: pointer;
        background: -moz-linear-gradient(top, red 0%, #ff0 17%, lime 33%, cyan 50%, blue 66%, #f0f 83%, red 100%);
        background: -webkit-gradient(linear, left top, left bottom, color-stop(0%, red), color-stop(17%, #ff0), color-stop(33%, lime), color-stop(50%, cyan), color-stop(66%, blue), color-stop(83%, #f0f), color-stop(100%, red));
        background: -webkit-linear-gradient(top, red 0%, #ff0 17%, lime 33%, cyan 50%, blue 66%, #f0f 83%, red 100%);
        background: -o-linear-gradient(top, red 0%, #ff0 17%, lime 33%, cyan 50%, blue 66%, #f0f 83%, red 100%);
        background: linear-gradient(to bottom, #ff0000 0%, #ffff00 17%, #00ff00 33%, #00ffff 50%, #0000ff 66%, #ff00ff 83%, #ff0000 100%)
    }}
}}

.lt-ie10 .colorpicker__1d {{
    {{
        background: url("/assets/settings/colorpicker-1d.100.png") no-repeat
    }}
}}

.colorpicker__1d-marker {{
    {{
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        -moz-box-shadow: 0 1px 2px rgba(0, 0, 0, 0.3);
        -webkit-box-shadow: 0 1px 2px rgba(0, 0, 0, 0.3);
        box-shadow: 0 1px 2px rgba(0, 0, 0, 0.3);
        border: 1px solid #ddd;
        position: absolute;
        background: #fff;
        width: 16px;
        height: 4px;
        margin: -2px 0 0 -1px;
        z-index: 2
    }}
}}

.disc {{
    {{
        -moz-border-radius: 100%;
        -webkit-border-radius: 100%;
        border-radius: 100%;
        overflow: hidden;
        position: relative;
        vertical-align: middle;
        display: inline-block;
        text-align: center
    }}
}}

.disc:before {{
    {{
        content: '';
        top: 0;
        right: 0;
        bottom: 0;
        left: 0;
        border: 2px solid #000;
        position: absolute;
        opacity: 0.15;
        border-radius: 100%;
        z-index: 1
    }}
}}

.disc--xs {{
    {{
        width: 16px;
        height: 16px;
        line-height: 16px
    }}
}}

.disc--xs .disc__wrap__img {{
    {{
        height: 16px
    }}
}}

.disc--s {{
    {{
        width: 30px;
        height: 30px;
        line-height: 30px
    }}
}}

.disc--s .disc__wrap__img {{
    {{
        height: 30px
    }}
}}

.disc--m {{
    {{
        width: 40px;
        height: 40px;
        line-height: 40px
    }}
}}

.disc--m .disc__wrap__img {{
    {{
        height: 40px
    }}
}}

.disc--l {{
    {{
        width: 50px;
        height: 50px;
        line-height: 50px
    }}
}}

.disc--l .disc__wrap__img {{
    {{
        height: 50px
    }}
}}

.disc--xl {{
    {{
        width: 60px;
        height: 60px;
        line-height: 60px
    }}
}}

.disc--xl .disc__wrap__img {{
    {{
        height: 60px
    }}
}}

.disc__wrap {{
    {{
        float: left;
        display: block;
        position: relative;
        text-align: center;
        left: 50%
    }}
}}

.disc__wrap__img {{
    {{
        vertical-align: middle;
        position: relative;
        display: inline;
        max-width: none;
        min-width: 1px;
        right: 50%
    }}
}}

.disc__img {{
    {{
        -moz-border-radius: 50%;
        -webkit-border-radius: 50%;
        border-radius: 50%;
        vertical-align: middle;
        display: inline-block;
        max-height: 100%;
        max-width: 100%;
        height: auto;
        width: auto
    }}
}}

.disc__obj {{
    {{
        position: absolute;
        margin: auto;
        bottom: 0;
        right: 0;
        left: 0;
        top: 0
    }}
}}

.frm {{
    {{
        display: block
    }}
}}

.frm__label {{
    {{
        font-size: 0.9em;
        font-weight: 600;
        color: #333
    }}
}}

.frm__label__chk {{
    {{
        margin-top: 0.4em;
        margin-right: 0.5em;
        display: inline-block;
        vertical-align: top
    }}
}}

.frm__label__txt {{
    {{
        display: inline-block;
        overflow: hidden
    }}
}}

.frm__text,
.frm__input {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        margin: 0;
        font-size: 1em;
        background-color: #f7f7f7;
        border: 1px solid #e6e6e6;
        color: #333;
        outline: none
    }}
}}

.frm__input--disabled {{
    {{
        background-color: #e6e6e6
    }}
}}

.frm__input {{
    {{
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        height: 2em;
        line-height: 2em;
        padding-left: 0.8em;
        padding-top: 0;
        padding-bottom: 0;
        -webkit-appearance: none
    }}
}}

.frm__input--clearable {{
    {{
        position: relative
    }}
}}

.frm__input--clearable .frm__input {{
    {{
        padding-right: 2em
    }}
}}

.frm__input--clearable .frm__input__clear {{
    {{
        position: absolute;
        top: 0;
        right: 0;
        font-size: .8em;
        line-height: 2.6em;
        padding: 0 1em;
        color: #888;
        cursor: pointer
    }}
}}

.frm__input--clearable .frm__input__clear:hover {{
    {{
        color: #333
    }}
}}

.frm__text {{
    {{
        padding: 0.8em;
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px
    }}
}}

.frm__select {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        display: inline-block;
        position: relative;
        overflow: hidden;
        line-height: 2.2;
        height: 2.35em;
        border: 1px solid #ddd;
        background-color: #fafafa;
        color: #333;
        padding: 0;
        vertical-align: middle;
        margin-bottom: 0;
        cursor: pointer
    }}
}}

.frm__select:hover {{
    {{
        background-color: #fff
    }}
}}

.frm__select select {{
    {{
        -webkit-appearance: none;
        -moz-appearance: none;
        -ms-appearance: none;
        -o-appearance: none;
        appearance: none;
        background: none;
        cursor: pointer;
        margin: 0;
        padding: 0 28px 0 0.75em;
        position: relative;
        display: block;
        font-size: 1em;
        line-height: inherit;
        min-width: 10em;
        width: 140% !important;
        height: 2.2em !important;
        outline: none !important;
        border: none !important
    }}
}}

.lt-ie9 .frm__select select {{
    {{
        padding-bottom: 0.4em
    }}
}}

.frm__select:after {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        content: "\76";
        margin-top: -5px;
        font-size: 12px;
        line-height: 1;
        pointer-events: none;
        vertical-align: middle;
        display: inline-block;
        position: absolute;
        right: 10px;
        top: 50%
    }}
}}

.frm__select--no-first select option:first-child {{
    {{
        display: none
    }}
}}

.frm__buttons {{
    {{
        clear: both;
        padding-top: 1em
    }}
}}

.frm__buttons .frm__btn {{
    {{
        margin-left: 0.5em
    }}
}}

.frm__btn {{
    {{
        min-width: 7em;
        height: 2em
    }}
}}

.frm__switch {{
    {{
        position: relative
    }}
}}

.frm__switch__inp {{
    {{
        border: 0;
        clip: rect(0 0 0 0);
        height: 1px;
        margin: -1px;
        overflow: hidden;
        padding: 0;
        position: absolute;
        width: 1px
    }}
}}

.frm__switch-on {{
    {{
        display: none
    }}
}}

.is-checked .frm__switch-on {{
    {{
        display: block
    }}
}}

.frm__switch-off {{
    {{
        display: block
    }}
}}

.is-checked .frm__switch-off {{
    {{
        display: none
    }}
}}

.frm__color__swatch {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        cursor: pointer;
        display: block;
        width: 50px;
        height: 2em;
        border: 1px solid #e6e6e6
    }}
}}

.frm__hr {{
    {{
        width: 100%;
        margin: 1em 0;
        border-bottom: 1px solid #eaeaea
    }}
}}

.frm--newsletter .frm__input {{
    {{
        -moz-border-radius: 0.25em;
        -webkit-border-radius: 0.25em;
        border-radius: 0.25em;
        vertical-align: top;
        width: 60%;
        max-width: 24em;
        height: 2.5em;
        line-height: 2;
        font-weight: 600
    }}
}}

.frm--newsletter .frm__btn {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        vertical-align: top;
        height: 2.5em
    }}
}}

.frm--newsletter__chks {{
    {{
        text-align: center;
        margin: 1.25em auto 0;
        font-size: 1em;
        min-width: 20em;
        width: 70%
    }}
}}

.frm--newsletter__chks .frm__label {{
    {{
        display: inline-block;
        margin: 0 1.25em;
        text-align: left;
        min-width: 8em
    }}
}}

.frm--newsletter__frq {{
    {{
        color: #888;
        display: block;
        font-size: 0.9em;
        font-weight: normal
    }}
}}

.frm--vrt .frm__label,
.frm--vrt .frm__input,
.frm--vrt .frm__text {{
    {{
        display: block;
        width: 100%
    }}
}}

.frm--vrt .frm__label {{
    {{
        margin-top: 1.25em;
        margin-bottom: 0.5em
    }}
}}

.loader {{
    {{
        display: none;
        width: 32px;
        height: 32px;
        background-size: 1600%;
        background-repeat: no-repeat
    }}
}}

.is-loading .loader {{
    {{
        display: block;
        -moz-animation: loader-animate 0.5s steps(15) infinite;
        -webkit-animation: loader-animate 0.5s steps(15) infinite;
        animation: loader-animate 0.5s steps(15) infinite
    }}
}}

@-moz-keyframes loader-animate {{
    {{
        0% {{
            {{
                background-position: left
            }}
        }}
        100% {{
            {{
                background-position: right
            }}
        }}
    }}
}}

@-webkit-keyframes loader-animate {{
    {{
        0% {{
            {{
                background-position: left
            }}
        }}
        100% {{
            {{
                background-position: right
            }}
        }}
    }}
}}

@keyframes loader-animate {{
    {{
        0% {{
            {{
                background-position: left
            }}
        }}
        100% {{
            {{
                background-position: right
            }}
        }}
    }}
}}

.msg--box,
.msg--info,
.msg--help,
.msg--untranslated,
.msg--warning,
#error_homepage,
.msg {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        display: block;
        position: relative;
        margin: 0.5em auto 1.25em;
        padding: 1em;
        color: #333
    }}
}}

.msg--box {{
    {{
        background: #f7f7f7
    }}
}}

.msg--info {{
    {{
        color: #a97022;
        background: #ffe7c9
    }}
}}

.msg--help,
.msg--untranslated {{
    {{
        color: #316c99;
        background: #d0e5f4
    }}
}}

.msg--warning,
#error_homepage {{
    {{
        color: #de5833;
        background: #f7d5cc
    }}
}}

.modal-trig {{
    {{
        max-height: 100%;
        position: relative;
        display: inline-block
    }}
}}

.modal {{
    {{
        pointer-events: none;
        text-align: center;
        font-weight: 400;
        line-height: 1.2
    }}
}}

.modal.is-showing {{
    {{
        pointer-events: auto
    }}
}}

.modal h1,
.modal h2,
.modal h3,
.modal h4,
.modal h5 {{
    {{
        padding: 0
    }}
}}

.modal p {{
    {{
        padding: 0.5em 0 0
    }}
}}

.modal__overlay {{
    {{
        position: absolute;
        margin: auto;
        bottom: 0;
        right: 0;
        left: 0;
        top: 0;
        display: none;
        height: 100%;
        width: 100%;
        z-index: 0
    }}
}}

.modal__box {{
    {{
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        background-color: #fff;
        color: #333;
        min-width: 160px;
        white-space: normal
    }}
}}

.modal__close {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        z-index: 1;
        line-height: 1;
        display: block;
        text-align: center;
        position: absolute;
        right: 1em;
        top: 1em;
        font-size: 14px;
        color: #8f8f8f
    }}
}}

.modal__close:hover {{
    {{
        color: #000;
        text-decoration: none
    }}
}}

.modal__header,
.modal__footer {{
    {{
        padding: 1em;
        background: #f7f7f7;
        border-bottom: 1px solid #e0e0e0
    }}
}}

.modal__header {{
    {{
        border-top-right-radius: 2px;
        border-top-left-radius: 2px
    }}
}}

.modal__footer {{
    {{
        border-bottom-right-radius: 2px;
        border-bottom-left-radius: 2px
    }}
}}

.modal__header__title {{
    {{
        font-weight: 600
    }}
}}

.modal__body {{
    {{
        padding: 1em;
        position: relative;
        display: block
    }}
}}

.modal__list {{
    {{
        padding: 0
    }}
}}

.modal__list__link {{
    {{
        display: block;
        color: #666;
        text-decoration: none;
        cursor: pointer
    }}
}}

.modal__list__link:visited,
.modal__list__link:focus {{
    {{
        color: #666
    }}
}}

.modal__list__link:active,
.modal__list__link:hover,
.modal__list__link.is-highlighted {{
    {{
        color: #666;
        background-color: #f7f7f7;
        text-decoration: none
    }}
}}

.modal__list__link.is-selected {{
    {{
        font-weight: 600
    }}
}}

.modal__list__link.is-selected:before {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        content: "\2713";
        position: relative;
        float: right;
        margin-left: 1em;
        top: -1px
    }}
}}

.modal__list__link.is-selected:hover,
.modal__list__link.is-selected:active {{
    {{
        font-weight: 600
    }}
}}

.modal__list__link.is-disabled {{
    {{
        color: #aaa;
        background-color: inherit
    }}
}}

.modal--popout {{
    {{
        position: absolute;
        margin: auto;
        bottom: 0;
        right: 0;
        left: 0;
        top: 0;
        display: block;
        height: 0;
        width: 0
    }}
}}

.modal--popout .modal__box {{
    {{
        -moz-transition: opacity 0.15s false, -moz-transform 0.15s false;
        -o-transition: opacity 0.15s false, -o-transform 0.15s false;
        -webkit-transition: opacity 0.15s false, -webkit-transform 0.15s false;
        transition: opacity 0.15s false, transform 0.15s false;
        -moz-box-shadow: 0 0 0.2px 1px rgba(0, 0, 0, 0.1);
        -webkit-box-shadow: 0 0 0.2px 1px rgba(0, 0, 0, 0.1);
        box-shadow: 0 0 0.2px 1px rgba(0, 0, 0, 0.1);
        -moz-transform: scale(0.85);
        -ms-transform: scale(0.85);
        -webkit-transform: scale(0.85);
        transform: scale(0.85);
        font-size: 14.4px;
        font-size: 1rem;
        width: 200px;
        position: absolute;
        display: block;
        bottom: auto;
        right: auto;
        left: auto;
        top: auto;
        visibility: hidden;
        opacity: 0
    }}
}}

.modal--popout .modal__box:before,
.modal--popout .modal__box:after {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        position: absolute;
        display: block;
        line-height: 24px;
        font-size: 24px;
        height: 24px;
        width: 24px
    }}
}}

.modal--popout .modal__box:before {{
    {{
        color: #e0e0e0
    }}
}}

.modal--popout .modal__box:after {{
    {{
        color: #fff
    }}
}}

.modal--popout.is-showing .modal__box {{
    {{
        -moz-transform: scale(1);
        -ms-transform: scale(1);
        -webkit-transform: scale(1);
        transform: scale(1);
        visibility: visible;
        z-index: 250;
        opacity: 1
    }}
}}

.modal--popout--top,
.modal--popout--bottom,
.modal--popout--bottom-left,
.modal--popout--bottom-right {{
    {{
        height: 100%
    }}
}}

.modal--popout--left,
.modal--popout--right {{
    {{
        width: 100%
    }}
}}

.modal--popout--sm .modal__box {{
    {{
        width: 160px
    }}
}}

.modal--popout--lg .modal__box {{
    {{
        width: 230px
    }}
}}

.modal--popout--top .modal__box,
.modal--popout--bottom .modal__box {{
    {{
        left: -100px
    }}
}}

.modal--popout--top.modal--popout--sm .modal__box,
.modal--popout--bottom.modal--popout--sm .modal__box {{
    {{
        left: -80px
    }}
}}

.modal--popout--top.modal--popout--lg .modal__box,
.modal--popout--bottom.modal--popout--lg .modal__box {{
    {{
        left: -115px
    }}
}}

.modal--popout--top .modal__box {{
    {{
        margin-bottom: 8px;
        bottom: 100%
    }}
}}

.modal--popout--top .modal__box:before,
.modal--popout--top .modal__box:after {{
    {{
        content: "\25bc";
        bottom: -17px;
        margin-left: -12px;
        left: 50%
    }}
}}

.modal--popout--top .modal__box:after {{
    {{
        margin-bottom: 1px
    }}
}}

.modal--popout--left .modal__box {{
    {{
        margin-right: 12px;
        right: 100%;
        top: -14px
    }}
}}

.modal--popout--left .modal__box:before,
.modal--popout--left .modal__box:after {{
    {{
        content: "\25b6";
        right: -17px;
        top: 4px
    }}
}}

.modal--popout--left .modal__box:after {{
    {{
        margin-right: 1px
    }}
}}

.modal--popout--right .modal__box {{
    {{
        margin-left: 12px;
        left: 100%;
        top: -14px
    }}
}}

.modal--popout--right .modal__box:before,
.modal--popout--right .modal__box:after {{
    {{
        content: "\25c0";
        left: -17px;
        top: 4px
    }}
}}

.modal--popout--right .modal__box:after {{
    {{
        margin-left: 1px
    }}
}}

.modal--popout--bottom .modal__box,
.modal--popout--bottom-right .modal__box,
.modal--popout--bottom-left .modal__box {{
    {{
        margin-top: 8px;
        top: 100%
    }}
}}

.modal--popout--bottom .modal__box:before,
.modal--popout--bottom .modal__box:after,
.modal--popout--bottom-right .modal__box:before,
.modal--popout--bottom-right .modal__box:after,
.modal--popout--bottom-left .modal__box:before,
.modal--popout--bottom-left .modal__box:after {{
    {{
        content: "\25b2";
        top: -17px;
        margin-left: -12px;
        left: 50%
    }}
}}

.modal--popout--bottom .modal__box:after,
.modal--popout--bottom-right .modal__box:after,
.modal--popout--bottom-left .modal__box:after {{
    {{
        margin-top: 1px
    }}
}}

.modal--popout--bottom.has-header .modal__box:after,
.modal--popout--bottom-right.has-header .modal__box:after,
.modal--popout--bottom-left.has-header .modal__box:after {{
    {{
        color: #f7f7f7
    }}
}}

.modal--popout--bottom-left .modal__box {{
    {{
        right: -23px;
        left: auto
    }}
}}

.modal--popout--bottom-left .modal__box:before,
.modal--popout--bottom-left .modal__box:after {{
    {{
        left: auto;
        margin-left: 0;
        right: 10px
    }}
}}

.modal--popout--bottom-right .modal__box {{
    {{
        left: -23px
    }}
}}

.modal--popout--bottom-right .modal__box:before,
.modal--popout--bottom-right .modal__box:after {{
    {{
        left: 10px;
        margin-left: 0
    }}
}}

.modal--popover {{
    {{
        -moz-transition: background 0.15s ease-out;
        -o-transition: background 0.15s ease-out;
        -webkit-transition: background 0.15s ease-out;
        transition: background 0.15s ease-out;
        display: table;
        overflow: hidden;
        visibility: hidden;
        background: rgba(255, 255, 255, 0);
        position: fixed;
        height: 100%;
        width: 100%;
        bottom: 0;
        right: 0;
        left: 0;
        top: 0
    }}
}}

.modal--popover.is-showing {{
    {{
        background: #ddd;
        background: rgba(255, 255, 255, 0.7);
        visibility: visible;
        z-index: 300
    }}
}}

.modal--popover.is-showing .modal__box {{
    {{
        -moz-transform: scale(1);
        -ms-transform: scale(1);
        -webkit-transform: scale(1);
        transform: scale(1);
        opacity: 1
    }}
}}

.modal--popover.is-showing .modal__overlay {{
    {{
        display: block
    }}
}}

.modal--popover .modal__wrap,
.modal--popover .modal__box {{
    {{
        z-index: 5;
        position: relative;
        overflow: hidden;
        margin: auto
    }}
}}

.modal--popover .modal__wrap {{
    {{
        pointer-events: none;
        display: table-cell;
        vertical-align: middle
    }}
}}

.modal--popover .modal__box {{
    {{
        -moz-transition: opacity 0.15s ease-out, -moz-transform 0.15s ease-out;
        -o-transition: opacity 0.15s ease-out, -o-transform 0.15s ease-out;
        -webkit-transition: opacity 0.15s ease-out, -webkit-transform 0.15s ease-out;
        transition: opacity 0.15s ease-out, transform 0.15s ease-out;
        -moz-transform: scale(0.85);
        -ms-transform: scale(0.85);
        -webkit-transform: scale(0.85);
        transform: scale(0.85);
        -moz-box-shadow: 0 0 15px -3px rgba(0, 0, 0, 0.35);
        -webkit-box-shadow: 0 0 15px -3px rgba(0, 0, 0, 0.35);
        box-shadow: 0 0 15px -3px rgba(0, 0, 0, 0.35);
        -webkit-overflow-scrolling: touch;
        pointer-events: auto;
        display: table;
        opacity: 0
    }}
}}

.modal--popover .modal__body {{
    {{
        max-height: 100%;
        overflow: auto;
        height: 100%
    }}
}}

.modal--popover--scroll {{
    {{
        display: block
    }}
}}

.modal--popover--scroll .modal__wrap,
.modal--popover--scroll .modal__box {{
    {{
        position: absolute;
        margin: auto;
        bottom: 0;
        right: 0;
        left: 0;
        top: 0;
        display: block
    }}
}}

.modal--popover--scroll .modal__box {{
    {{
        overflow: auto
    }}
}}

.modal--popover--notransition {{
    {{
        -moz-transition: none 0.3s ease-in-out 0s;
        -o-transition: none 0.3s ease-in-out 0s;
        -webkit-transition: none 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: none 0.3s ease-in-out 0s
    }}
}}

.modal--popover--notransition .modal__box {{
    {{
        -moz-transition: none 0.3s ease-in-out 0s;
        -o-transition: none 0.3s ease-in-out 0s;
        -webkit-transition: none 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: none 0.3s ease-in-out 0s;
        -moz-transform: none;
        -ms-transform: none;
        -webkit-transform: none;
        transform: none;
        opacity: 1
    }}
}}

.modal--popover--dk.is-showing {{
    {{
        background: rgba(210, 210, 210, 0.6)
    }}
}}

.nav {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        -moz-border-radius: 50%;
        -webkit-border-radius: 50%;
        border-radius: 50%;
        background: #e6e6e6;
        background: rgba(0, 0, 0, 0.1);
        color: #f8f8f8;
        position: absolute;
        top: 50%;
        margin-top: -21.33333px;
        width: 64px;
        height: 64px;
        line-height: 64px;
        font-size: 28px;
        vertical-align: middle;
        text-align: center;
        display: block;
        z-index: 2;
        cursor: pointer
    }}
}}

.nav:hover {{
    {{
        background: rgba(0, 0, 0, 0.25)
    }}
}}

.nav:active {{
    {{
        background: rgba(0, 0, 0, 0.5)
    }}
}}

.is-mobile .nav,
.touch .nav,
.no-js .nav {{
    {{
        display: none
    }}
}}

.nav--hero {{
    {{
        -moz-box-shadow: inset 0 0 0 2px #fff;
        -webkit-box-shadow: inset 0 0 0 2px #fff;
        box-shadow: inset 0 0 0 2px #fff;
        background: none;
        border: 2px solid #fff;
        border-color: transparent
    }}
}}

.nav--hero:hover {{
    {{
        background: none
    }}
}}

.nav--prev {{
    {{
        text-indent: -4px;
        left: 24px
    }}
}}

.nav--prev:before {{
    {{
        content: "\2039";
        line-height: 1
    }}
}}

.nav--next {{
    {{
        text-indent: 4px;
        right: 24px
    }}
}}

.nav--next:before {{
    {{
        content: "\203a";
        line-height: 1
    }}
}}

.nav-menu,
.nav-menu--slideout {{
    {{
        background-color: #f2f2f2;
        background-color: rgba(242, 242, 242, 0.975)
    }}
}}

.nav-menu ul,
.nav-menu--slideout ul {{
    {{
        padding-top: 0;
        padding-bottom: 0;
        list-style: none
    }}
}}

.nav-menu li,
.nav-menu--slideout li {{
    {{
        list-style: none
    }}
}}

.nav-menu__icon,
.nav-menu__close {{
    {{
        text-align: center;
        color: #bfbfbf;
        font-size: 1.2em;
        line-height: 1;
        background-color: transparent
    }}
}}

.nav-menu__icon--top-right,
.nav-menu__close {{
    {{
        position: absolute;
        top: 0;
        right: 0
    }}
}}

.nav-menu__icon--clickable,
.nav-menu__close {{
    {{
        -moz-transition: all 0.15s ease-in-out 0s;
        -o-transition: all 0.15s ease-in-out 0s;
        -webkit-transition: all 0.15s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: all 0.15s ease-in-out 0s;
        cursor: pointer;
        -webkit-tap-highlight-color: transparent;
        z-index: 2
    }}
}}

.nav-menu__icon--clickable:hover,
.nav-menu__close:hover,
.nav-menu__icon--clickable:focus,
.nav-menu__close:focus {{
    {{
        color: #575757
    }}
}}

.nav-menu__icon--clickable:active,
.nav-menu__close:active {{
    {{
        color: #bfbfbf
    }}
}}

.nav-menu__icon--clickable:visited,
.nav-menu__close:visited {{
    {{
        color: #bfbfbf
    }}
}}

.nav-menu__close {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        line-height: 1;
        padding: 1.6em 1em 1em
    }}
}}

.nav-menu__list {{
    {{
        display: block;
        position: relative;
        list-style: none;
        padding: 0;
        margin: 0
    }}
}}

.nav-menu__list li {{
    {{
        position: relative;
        top: -0.45em
    }}
}}

.nav-menu__heading {{
    {{
        padding-top: 2.2em;
        padding-bottom: 1.6em;
        margin-bottom: -1.25em;
        color: #888
    }}
}}

.nav-menu__heading span {{
    {{
        text-transform: uppercase;
        font-size: 0.75em
    }}
}}

.is-mobile .nav-menu__heading {{
    {{
        margin-bottom: -1.5em
    }}
}}

.nav-menu__heading--primary {{
    {{
        padding-top: 2em;
        margin-bottom: -0.75em
    }}
}}

.nav-menu__item {{
    {{
        position: relative;
        font-size: 0.9176em
    }}
}}

.is-mobile .nav-menu__item {{
    {{
        padding: 0.125em 0;
        top: -0.25em
    }}
}}

.nav-menu__item>a {{
    {{
        padding: 0;
        display: block
    }}
}}

.nav-menu__item,
.nav-menu__item>a,
.nav-menu__item>a:visited {{
    {{
        color: #333
    }}
}}

.nav-menu__item>a:hover {{
    {{
        color: #333;
        text-decoration: underline
    }}
}}

.nav-menu__item.only--mob {{
    {{
        display: none
    }}
}}

.is-mobile .nav-menu__item.only--mob {{
    {{
        display: inline-block
    }}
}}

.nav-menu__item--primary {{
    {{
        padding-top: 2.05em;
        margin-bottom: -0.75em
    }}
}}

.nav-menu__item--icon {{
    {{
        margin-top: 5px;
        margin-bottom: 5px
    }}
}}

.nav-menu__item--icon:first-child {{
    {{
        margin-top: 0
    }}
}}

.nav-menu__item--icon:last-child {{
    {{
        margin-bottom: 0
    }}
}}

.nav-menu__item__icon {{
    {{
        margin-right: 5px
    }}
}}

.nav-menu__themes {{
    {{
        height: 17px;
        padding: 12px 0 12px !important
    }}
}}

.nav-menu__theme {{
    {{
        -moz-border-radius: 13px;
        -webkit-border-radius: 13px;
        border-radius: 13px;
        display: block;
        width: 18px;
        height: 18px;
        padding: 3px;
        float: left;
        margin-right: 5px;
        cursor: pointer
    }}
}}

.nav-menu__theme.theme-is-selected {{
    {{
        padding: 2px;
        border: 1px solid #333
    }}
}}

.nav-menu__theme-color {{
    {{
        -moz-border-radius: 10px;
        -webkit-border-radius: 10px;
        border-radius: 10px;
        display: block;
        position: relative;
        width: 18px;
        height: 18px;
        overflow: hidden;
        z-index: 1
    }}
}}

.nav-menu__theme-color-top,
.nav-menu__theme-color-bot {{
    {{
        -moz-transform: rotate(45deg);
        -ms-transform: rotate(45deg);
        -webkit-transform: rotate(45deg);
        transform: rotate(45deg);
        position: absolute;
        display: block;
        width: 18px;
        height: 18px
    }}
}}

.nav-menu__theme-color-top {{
    {{
        left: -5px;
        top: -5px
    }}
}}

.nav-menu__theme-color-bot {{
    {{
        right: -6px;
        bottom: -6px
    }}
}}

.nav-menu--slideout {{
    {{
        -moz-transition: -moz-transform 0.3s ease-in-out 0s;
        -o-transition: -o-transform 0.3s ease-in-out 0s;
        -webkit-transition: -webkit-transform 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: transform 0.3s ease-in-out 0s;
        -moz-transform: translate3d(14.25em, 0, 0);
        -webkit-transform: translate3d(14.25em, 0, 0);
        transform: translate3d(14.25em, 0, 0);
        -moz-backface-visibility: hidden;
        -webkit-backface-visibility: hidden;
        backface-visibility: hidden;
        -moz-transform-style: preserve-3d;
        -webkit-transform-style: preserve-3d;
        transform-style: preserve-3d;
        -moz-box-shadow: -1px 0 1px rgba(0, 0, 0, 0.2);
        -webkit-box-shadow: -1px 0 1px rgba(0, 0, 0, 0.2);
        box-shadow: -1px 0 1px rgba(0, 0, 0, 0.2);
        width: 14em;
        display: block;
        height: 100%;
        position: fixed;
        top: 0;
        bottom: 0;
        right: -14.25em;
        left: auto;
        z-index: 200
    }}
}}

.nav-menu--slideout .nav-menu__list {{
    {{
        padding-left: 1.5em;
        position: absolute;
        overflow: auto;
        overflow-x: hidden;
        bottom: 0;
        right: 0;
        left: 0;
        top: 0
    }}
}}

.nav-menu--slideout .nav-menu__list:after {{
    {{
        content: "";
        clear: both;
        display: block;
        height: 15em
    }}
}}

.nav-menu--slideout.is-open {{
    {{
        right: 0;
        -moz-transform: translate3d(0, 0, 0);
        -webkit-transform: translate3d(0, 0, 0);
        transform: translate3d(0, 0, 0)
    }}
}}

.csstransforms3d .nav-menu--slideout {{
    {{
        right: 0
    }}
}}

.is-mobile-device .nav-menu--slideout,
.has-search-focus .site-wrapper--home .nav-menu--slideout {{
    {{
        position: absolute
    }}
}}

.notification {{
    {{
        -moz-transition: all 0.2s ease-out;
        -o-transition: all 0.2s ease-out;
        -webkit-transition: all 0.2s ease-out;
        transition: all 0.2s ease-out;
        display: table;
        left: 0;
        opacity: 0;
        position: fixed;
        top: -64px;
        width: 100%;
        z-index: 500
    }}
}}

.notification.is-showing {{
    {{
        opacity: 0.8;
        top: 40px
    }}
}}

.notification__wrap {{
    {{
        display: table-cell;
        width: 100%
    }}
}}

.notification__text {{
    {{
        -moz-border-radius: 5px;
        -webkit-border-radius: 5px;
        border-radius: 5px;
        background: #333;
        color: #fff;
        display: table;
        margin: 0 auto;
        max-width: 300px;
        padding: 10px 45px;
        text-align: center
    }}
}}

.sep--before:before,
.sep--after:after,
.sep--small,
.sep,
.zcm__sep {{
    {{
        border-left: 1px solid #d0d0d0;
        margin: 0 1em;
        display: inline-block;
        vertical-align: baseline;
        position: relative;
        top: 0.15em;
        float: none;
        height: 1em;
        width: 0
    }}
}}

.sep--before:before,
.sep--after:after {{
    {{
        content: ""
    }}
}}

.sep--small {{
    {{
        border-color: #e5e5e5;
        height: 0.75em;
        margin: 0 0.35em;
        top: 0
    }}
}}

.switch {{
    {{
        -moz-border-radius: 10px;
        -webkit-border-radius: 10px;
        border-radius: 10px;
        position: relative;
        height: 16px;
        width: 26px;
        background: #aaa;
        cursor: pointer
    }}
}}

.switch.is-on {{
    {{
        background: #4495d4
    }}
}}

.switch__knob {{
    {{
        -moz-border-radius: 8px;
        -webkit-border-radius: 8px;
        border-radius: 8px;
        -moz-transition: 0.1s, linear, left 0.3s ease-in-out 0s;
        -o-transition: 0.1s, linear, left 0.3s ease-in-out 0s;
        -webkit-transition: 0.1s, linear, left 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: 0.1s linear left 0.3s ease-in-out 0s;
        position: absolute;
        display: block;
        left: 2px;
        top: 2px;
        height: 12px;
        width: 12px;
        background-color: #fff
    }}
}}

.is-on .switch__knob {{
    {{
        left: 12px
    }}
}}

@font-face {{
    {{
        font-family: 'ddg-serp-icons';
        src: url("/font/ddg-serp-icons.eot?v=120");
        src: url("/font/ddg-serp-icons.eot?v=120#iefix") format("embedded-opentype"), url("/font/ddg-serp-icons.svg?v=120#ddg-serp-icons") format("svg"), url("data:application/x-font-woff;charset=utf-8;base64,d09GRgABAAAAACIIAAsAAAAAIbwAAQAAAAAAAAAAAAAAAAAAAAAAAAAAAABPUy8yAAABCAAAAGAAAABg9/SNIGNtYXAAAAFoAAAERAAABETdgHNaZ2FzcAAABawAAAAIAAAACAAAABBnbHlmAAAFtAAAGGAAABhgQb0q5mhlYWQAAB4UAAAANgAAADYIcO6UaGhlYQAAHkwAAAAkAAAAJAfTBFVobXR4AAAecAAAAPwAAAD88ekammxvY2EAAB9sAAAAgAAAAIC2Arv8bWF4cAAAH+wAAAAgAAAAIABLALZuYW1lAAAgDAAAAdoAAAHajy6dsnBvc3QAACHoAAAAIAAAACAAAwAAAAMD9wGQAAUAAAKZAswAAACPApkCzAAAAesAMwEJAAAAAAAAAAAAAAAAgAAAAwAA6OAAAAAAAAAAAAAAAAAAQAAA//8DwP/AAEADwABAAAAAAQAAAAAAAAAAAAAAIAAAAAAABgAAAAMAAAA0AAAABAAAAdwAAQADAAAANAABAAQAAAHcAAMAAQAAADQAAwAKAAAB3AAEAagAAABmAEAABQAmAAEAIQApACsALQA8AD4AQABDAEcASQBPAFMAVQBYAF4AZABpAG4AdgCrALsA1yAmIDohkyGpIdEh9iKVIp0jAiVRJbIltyW6JbwlwSXPJgUmESYpJjcmOSZlJmsnEykTKwb//f//AAAAAAAgACgAKwAtADwAPgBAAEMARwBJAE8AUwBVAFgAXgBjAGkAbgB1AKsAuwDXICYgOSGQIakh0SH2IpUinSMCJVElsiW2JbolvCXAJc8mBSYRJikmNyY5JmUmaycTKRIrBv/9//8AAf/j/93/3P/b/83/zP/L/8n/xv/F/8D/vf+8/7r/tf+x/63/qf+j/2//YP9F3/ff5d6Q3nveVN4w3ZLdi90n2tnaedp22nTac9pw2mPaLtoj2gzZ/9n+2dPZztkn1ynVNwADAAEAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAADAAAAAACaAAAAAAAAAAyAAAAAAAAAAEAAAABAAAAIAAAACEAAAADAAAAKAAAACkAAAAFAAAAKwAAACsAAAAHAAAALQAAAC0AAAAIAAAAPAAAADwAAAAJAAAAPgAAAD4AAAAKAAAAQAAAAEAAAAALAAAAQwAAAEMAAAAMAAAARwAAAEcAAAANAAAASQAAAEkAAAAOAAAATwAAAE8AAAAPAAAAUwAAAFMAAAAQAAAAVQAAAFUAAAARAAAAWAAAAFgAAAASAAAAXgAAAF4AAAATAAAAYwAAAGQAAAAUAAAAaQAAAGkAAAAWAAAAbgAAAG4AAAAXAAAAdQAAAHYAAAAYAAAAqwAAAKsAAAAaAAAAuwAAALsAAAAbAAAA1wAAANcAAAAcAAAgJgAAICYAAAAdAAAgOQAAIDoAAAAeAAAhkAAAIZMAAAAgAAAhqQAAIakAAAAkAAAh0QAAIdEAAAAlAAAh9gAAIfYAAAAmAAAilQAAIpUAAAAnAAAinQAAIp0AAAAoAAAjAgAAIwIAAAApAAAlUQAAJVEAAAAqAAAlsgAAJbIAAAArAAAltgAAJbcAAAAsAAAlugAAJboAAAAuAAAlvAAAJbwAAAAvAAAlwAAAJcEAAAAwAAAlzwAAJc8AAAAyAAAmBQAAJgUAAAAzAAAmEQAAJhEAAAA0AAAmKQAAJikAAAA1AAAmNwAAJjcAAAA2AAAmOQAAJjkAAAA3AAAmZQAAJmUAAAA4AAAmawAAJmsAAAA5AAAnEwAAJxMAAAA6AAApEgAAKRMAAAA7AAArBgAAKwYAAAA9AAHzyAAB88gAAAA+AAEAAf//AA8AAQAAAAAAAAAAAAIAADc5AQAAAAABAAAAAAAAAAAAAgAANzkBAAAAAAEAAAAAAAAAAAACAAA3OQEAAAAAAwBAAAADwAOAABAAFQAaAAABISIGFREUFjMhMjY1ETQmIwUzESMREyM1MxUDQP2ANUtLNQKANUtLNf5/f3+Af38DgEs1/YA1S0s1AoA1S8D+5gEa/gN9fQAAAAAFAAD/wAQAA8AAFAApAEwAWQBmAAAFIi4CNTQ+AjMyHgIVFA4CIxEiDgIVFB4CMzI+AjU0LgIjEwYiJzgBMS4BIgYHOAExBiInJjQ3OAE5AT4BMhYXOQEWFAcDIiY1NDYzMhYVFAYjIyImNTQ2MzIWFRQGIwIAaruLUFCLu2pqu4tQUIu7aleZcUJCcZlXV5lxQkJxmVfpDiUOI1ZZViMOJQ4ODi94fnkvDg51HCkpHBwqKhzoHCoqHBwpKRxAUIu7amq7i1BQi7tqaruLUAOjQnGZV1eZcUJCcZlXV5lxQv1dDg4hISEhDg4OJQ4vLy8vCikOAQAqHBwqKhwcKiocHCoqHBwqAAAFAAD/wAQAA8AAFAApAE4AWwBoAAAFIi4CNTQ+AjMyHgIVFA4CIxEiDgIVFB4CMzI+AjU0LgIjEzEOASImJzE4ATEmNDc2Mhc4ATEeATI2NzgBMTYyFxYUBzgBMSciJjU0NjMyFhUUBiMjIiY1NDYzMhYVFAYjAgBqu4tQUIu7amq7i1BQi7tqV5lxQkJxmVdXmXFCQnGZV+QveH55Lw4ODiUOI1ZZViMOJQ4ODnAcKSkcHCoqHOgcKiocHCkpHEBQi7tqaruLUFCLu2pqu4tQA6NCcZlXV5lxQkJxmVdXmXFC/aMvLy8vDiUODg4hISEhDg4KKQ66KhwcKiocHCoqHBwqKhwcKgABAMAAgANAAwAADAAAASERIxEhFSERMxEhNQNA/wCA/wABAIABAAIAAQD/AID/AAEAgAAAAQDAAYADQAIAAAQAAAE1IRUhA0D9gAKAAYCAgAAAAAABAP4AFQMDA2kABwAAARcJAQcBMScCqVr+sQFPWv6wWwNpWv6w/rFbAVBaAAEA/gAVAwMDaQAHAAAJAQcJARcBMwMD/lZbAVD+sFsBTwEBvwGqWv6w/rFbAVAAAAIAwQAqA0ADgAASAB8AAAEiDgIVFBYXCQE+ATU0LgIjESImNTQ2MzIWFRQGIwIBQ3RXMhcUARUBFBUWMld0QjZLSzY1S0s1A4AxVHFBKk8i/nwBhCJPKkFxVDH+Q083OE9PODdPAAAAAwCAAEADgANAABQAKQAvAAABIg4CFRQeAjMyPgI1NC4CIxEiLgI1ND4CMzIeAhUUDgIjERUjFTMRAgBPjGk8PGmLUE+MaTw8aYtQOWNJKytJYzk4Y0orK0pjOIDAA0A8aYtQT4xpPDxpi1BPjGk8/XMrSWM5OGNKKytKYzg5Y0oqAc3AQAEAAAkAAACAA0ADwAAEAAkADgATABgAHQAiACcALAAAEzMVIzUhMxUjNSEzFSM1ATMVIzUhMxUjNSEzFSM1ATMVIzUhMxUjNSEzFSM1AMDAAUDAwAFAwMD9gMDAAUDAwAFAwMD9gMDAAUDAwAFAwMACgMDAwMDAwAFAwMDAwMDA/YDAwMDAwMAAAAADAAD/wAQAA8AAFAAZAB4AAAEiDgIVFB4CMzI+AjU0LgIjEyMRMxERIzUzFQIAaruLUFCLu2pqu4tQUIu7akCAgICAA8BQi7tqaruLUFCLu2pqu4tQ/QABQP7AAYCAgAABAAz/zAP9A3IAHAAAATIeAhUUDgIjIg4CMTA+ASYnLgE1ND4CMwIFaLiJT0+JuGgjgH5eKR0LMz1FT4m4aQNyQG+UVFWAVy05RDk9XXAyN01PVJRvQAAAAgCW/9oD7QMyACAANQAAJSc+AS4BJy4BIgYHDgEUFhceAjY3OAExFxYyPwE2NCclLgE0Njc+ATIWFx4BFAYHDgEiJicD7bsrJA4+OECgp6BAQD8/QDiIkZE/uxEwESkREfzqMzIyMzJ+hH8yMjMzMjJ/hH4yVLpAkJKKOD8/Pz9An6efQDc+DiIpuxERKREvEboyfoN+MzIyMjIzfoN+MjIyMjIAAAAAAgAAAEAEAANAAB0AOAAAJRUhNSc1MCY1PAExMDYzMDI7ATIWMTAUFRQGMRUXJTUwNjU8ATU0JisBIgYVHAEVFBYxFQUVITUnBAD/AEBAHk0MCRVNHkDA/cJAL24Hbi5A/v4CwP6bW5slQCAgIGCAgGAgICBBZKWAHyEhjhERb28REY4hIR+AgICBfwABAF8AHwOhA2EAIAAACQE2NCcmIgcJASYiBwYUFwkBBhQXFjI3CQEWMjc2NCcBAjEBcAoKCh0K/pD+kAodCgoKAXD+kAoKCh0KAXABcAodCgoK/pABwAFwCh0KCgr+kAFwCgoKHQr+kP6QCh0KCgoBcP6QCgoKHQoBcAAAAQCZAOgDaAKYAAUAAAkBJwkBBwH//vRaAWYBaFkB9P71VwFY/qdXAAEAAACABAADGQAsAAABMCYHDgExMA4CFR4DMSE1IzcXIxUhMD4CNzYuAjEwLgInJg4CMQF3Uz84BjQ/NAE5RDcBC0uLnV0BAzRDPQgFMT41AiVcWk1nPhoCby8kI2AQLE9AQEYgBsCgoMAFHkI9QlMuEFNoXgsIM0Q7AAABANsAUgNuAy4AEQAACQIVIgYHESMRMzU0PgIzFQJJASX+2z92JpOTIjxQLQEuAQABALcmIwEA/SS3LU45IK8ABABUAK4DqwLMABQAKQA2AEMAAAEiDgIHMB4CMzI+AjEwLgIjAyIuAjU0PgIzMh4CFRQOAiMDIgYVFBYzMjY1NCYjFwYmJy4BNzYWFx4BBwIFToBqViNGd5pUVZp3Rkl4l04HKEc1Hh41RygoRzQfHjVHKAMwREQwMUREMU4GFwoLBQcGGAoKBQcCzDhSXCRWaFZWaFZTZFP+Lh80RygpRjUeHjVGKShHNB8BOEQwMEREMDBEVgcFCgoYBgcFCgoYBgAAAAsAIAAwA+ADUAAbACwAOwBKAFkAaAB3AIYAlQCkALMAAAEhIgYVERQGIyImNREiBhURFBYzITI2NRE0JiMFNDYzITIWHQEUBiMhIiY9AQEhIiY1NDYzITIWFRQGIzUhIiY1NDYzITIWFRQGIzUhIiY1NDYzITIWFRQGIwUjIiY1NDY7ATIWFRQGIzUjIiY1NDY7ATIWFRQGIzUjIiY1NDY7ATIWFRQGIzUjIiY1NDY7ATIWFRQGIzUjIiY1NDY7ATIWFRQGIzUjIiY1NDY7ATIWFRQGIwOr/RUWHxALCxAWHzEfAx4gMh8W/TAQCwEKCw8PC/72CxABJf71Cw8PCwELCxAQC/71Cw8PCwELCxAQC/71Cw8PCwELCxAQCwF18AsPDwvwCxAQC/ALDw8L8AsQEAvwCw8PC/ALEBAL8AsPDwvwCxAQC/ALDw8L8AsQEAvwCw8PC/ALEBALA1AfFv2ACxAQCwJKHxb90R8yMCECmhYfhQsQEAvWCxAQC9b90A8LCxAQCwsPahALCxAQCwsQaxALCw8PCwsQ1Q8LCxAQCwsPahALCxAQCwsQaxALCw8PCwsQaw8LCxAQCwsPahALCxAQCwsQaxALCw8PCwsQAAAAAAEAgABAA4ADQQAgAAABMCYzMhYxMBQWFBUUBjc2BjEFFSEnJTUwJjU8ATEwNjMB5gY+Xi8BOQgBAQEF/QICAQ0wMFkDPwKRMUA/DRsfCAJrtmJguWgQIiKbjwAAAQCZAOgDaAKYAAUAAAkBBwkBJwH//vRaAWYBaFkBjAELV/6oAVlXAAEA5gA3AqsDCwAHAAATARcJAQcBMeYBalv+8AEQW/7xAaEBalr+8P7xWwEQAAAAAAEBVQA3AxoDCwAHAAAJAQcJARcBMQMa/pZbARD+8FsBDwGhAWpa/vD+8VsBEAAAAAEAlgBWA2oDKgALAAABJwkBBwkBFwkBNwEDalr+8P7xWwEP/vFaARABD1v+8QLQWv7xAQ9a/vD+8FoBD/7xWgEQAAAAAwBAAYADgAJAAAwAGQAmAAATMhYVFAYjIiY1NDYzITIWFRQGIyImNTQ2MyEyFhUUBiMiJjU0NjOgKDg4KCg4OCgBQCg4OCgoODgoAUAoODgoKDg4KAJAOCgoODgoKDg4KCg4OCgoODgoKDg4KCg4AAEA1f+sAxEDqgAGAAAFNwkBJwkBAtQ8/j4Bwzz+AAH/VDwBwwHDPP4B/gEAAQDE/6sDAAOpAAYAAAEHCQEXCQEBATwBw/48PAIA/gEDqTz+Pv48PAIAAf4AAAAAAQAbABsD5QNvAAgAACUBITUhAScJAQHy/qMDUPywAV0t/lYBqkgBXUABXS3+Vv5WAAAAAAEAVv/gA6oDqgAIAAAJAhcBETMRAQOq/lb+Vi0BXUABXQIAAar+Vi0BXfywA1D+owAAAAABABsAGwPlA28ACAAACQEHASEVIQEXA+X+Vi0BXfywA1D+oy0BxQGqLf6jQP6jLQABAFb/4AOqA6oACAAAAScBESMRAQcBA6ot/qNA/qMtAaoBii3+pANP/LEBXC3+VgAEAAAAAAQAA8AABwAMABQAGQAAATUhNSE1CQElMzUjFQE1ITUhNQkBASMVMzUCwP6AAYABQP7A/YCAgAEAAYD+gP7AAUACgICAAcDAgMD/AP8AwICA/YDAgMD/AP8AAUCAgAAAAQCsAE8DHwNPABoAAAEFMDQVDgMVFBYXLgM1ND4CNxQ0MQUDH/7iIVFHMGJKO2dLK09tdCUBHgJf8JICARg2V0BObQ8INVFqPV55RhwCAZHwAAAAAwAAAEADwANAAAQACQAOAAATIRUhNREhFSE1ESEVITUAA8D8QAPA/EADwPxAA0CAgP7AgID+wICAAAAAAgAA/8AEAAPAABQAIQAAASIOAhUUHgIzMj4CNTQuAiMBIxUjNSM1MzUzFTMVAgBqu4tQUIu7amq7i1BQi7tqAQDAgMDAgMADwFCLu2pqu4tQUIu7amq7i1D9wMDAgMDAgAAAAAIAAP/ABAADwAAUABkAAAEiDgIVFB4CMzI+AjU0LgIjASE1IRUCAGq7i1BQi7tqaruLUFCLu2oBAP4AAgADwFCLu2pqu4tQUIu7amq7i1D9wICAAAEAdABAA4wDDAAKAAABMwkBMxEzETMRMwMAjP50/nSMwIDAAUABzP40/wABAP8AAAIAwgCCAz4C/gAEAAkAAAEzESMRITMRIxECd8fH/kvHxwL+/YQCfP2EAnwAAQDeAQADIgKHAAMAAAkBIQECAAEi/bwBIgKH/nkBhwABAUAAngLHAuIAAwAAJREJAQFAAYf+eZ4CRP7e/t4AAAIAAP/ABAADwAAUAB0AAAEiDgIVFB4CMzI+AjU0LgIjEwcnNyc3HwEHAgBqu4tQUIu7amq7i1BQi7tqwedapaVa5xkZA8BQi7tqaruLUFCLu2pqu4tQ/ebnW6amW+caGgAAAAABAP8AeANmAwgAAwAACQERAQNm/ZkCZwHA/rgCkP64AAEA3gD5AyICgAADAAATIQkB3gJE/t7+3gKA/nkBhwAAAQE5AJ4CwALiAAMAAAkBEQEBOQGH/nkBwAEi/bwBIgACAAD/wAQAA8AAFAAdAAABMh4CFRQOAiMiLgI1ND4CMwMXNyc3Jw8BFwIAaruLUFCLu2pqu4tQUIu7asHnWqWlWucZGQPAUIu7amq7i1BQi7tqaruLUP3m51umplvnGhoAAAAAAQAA/8AEAAPAABQAAAEiDgIVFB4CMzI+AjU0LgIjAgBqu4tQUIu7amq7i1BQi7tqA8BQi7tqaruLUFCLu2pqu4tQAAABAFYAJAOqA08ACgAAARMFBxMlBRMnJRMCAG0BPfpX/vn++Vf6AT1tA0/+1gzD/s6ysgEywwwBKgAAAAACAMQAoQMgAv0AFAAdAAABLgEiBgcOARQWFx4BMjY3PgE0JicBByc3FwEXARcDID6epJ0/Pz4+Pz+dpJ4+Pz8/P/7nVtRVfgECVv7+AQL9Pz4+Pz6epJ4+Pz4+Pz6epJ4+/jdV1VV+AQJW/v4BAAAAAQAA/8AEAAPAABcAAAEnFSM1NycHFxUhNQcXNSEDIxc3IxMzFQQAwP9/wMCB/v/AwAEBAYDAwIAB/wHAwID+AsDAAv6AwMCA/wDAwAEAgAADAET/3QQRA6MAFABcAHoAAAEiDgIVFB4CMzI+AjU0LgIjESIGFRQOAhUUFjMyFhUUBgcOAQcGJic0JjE8ATU0JicuASc+ATc+ARUUBhUUFjMyNjU0NjMyBjMyNiMiJjc+ATceARUUBiMBIi4CIyImNTQ2MzI2NTQ2Fx4BFRQWFxQGBw4BIwIqZLGETU2EsWRlsYRNTYSxZRQXMz4zNSssb1gXAxYDGgsBAT0cTFgODlc2JDk9DxQUEgUZGgoXKAUoIgcmCh8KSExsFQEyEQsLFRxFEC4UFAMrERIpLwoKDg0mEQOjTIOwZGSwg0xMg7BkZLCDTP76RDEYHBEPCxcdckBAOh8EFwMbKhkBAR9YODlJCh05KUZ8KhgHExU0FxcgHBcXJDdqLwoBAgEBMSZCGP5JOEM4cw8QElcXJykYF1U2LjEiJkoiGi4AAAYAQAAAA8ADgAAMABkANQBKAGkAngAAARQGIyImNTQ2MzIWFSEUBiMiJjU0NjMyFhUHIgYHBhYXFjY3PgEzMhYXHgEzMjY3PgEnLgEjESIOAhUUHgIzMj4CNTQuAiMRIi4CNTQ+AjMyHgIXBw4BFRQWMzoBMw4DIwE4ATEOAQc4ASMOASMwIiMGIiMwBiMqASMiJjU0NjceARcWFDMeARcUMjEeARcxHgEVFAYHAbsoHBwoKBwcKAESKBwcKCgcHCjNSHkiCgkQESQKFVEvL1EVBhUMBgwGEAkKInlIXaN6RkZ6o11do3pGRnqjXUyFYzo6Y4VMNmRVRBYSDFNDMAIFAxREWWo6AWYBAwIBAQMBAQEBAgIBAQEEARojJRgBBAIBAQQIAwECBAEMEBIPAjEcKCgcHCgoHBwoKBwcKCgckj82ESQKCgkQIigoIgsLAwQKJBE2PwHhRnqjXV2jekZGeqNdXaN6RvzSOmOFTEyFYzoeN0wtEw1oKjBDM1U+IwEmAQEBAQEBASQZDjocAgUCAQEFCgUBAwUDER8JEh0HAAAAAAEAIABFA3YDMQAkAAABMhcWFRQHBg8BCQExJyYnJicmNzQ3NjMyHwE2NzY3Njc2NzYzAqFbPT0bGhsb/sD+tBoZBwgSEgc8PVtcZxQECAcYGBgYIiIfAzE0NG8zNTUcHP7AAUsfHhEQKikZbzQ0bhUFCAkXFxERDg8AAAABAFEAMQOAA1IAOQAANyImJyY2NzYyMzIWHwERJREcARUWBgcOAQciBiMxIiYnJjY3MjYzMhYfAREFERwBFRYGBw4BBwYiI+Y7VgQFTEIFCgUPLA4JAkACExQVOCAFCgU7VgQFVUIFCgUPLw4J/kACDRQVMiAFCgUxPS4zUAYBBAUDAfs9/YoBAwEYLhQTGQMBPi4yUAcBBQQDAW8s/kYBAwEYLxMUGAMBAAAAAQCXAJUDcgLrABgAABMGFh8BHgE3AT4BLwEmBgcBBiYvAS4BDwGXDwYQ4RA3EAGTEQgQQRAqEf7XEBsQaxAvEDEBzREwEOURARAByBAxED0QAxD+pxACEG4QAhEzAAAAAgCAAEADgANAAA4AKQAAATQmIyEiBhUUFjMhMjY1AQYUFxYyNzE3ERQWMzI2NREXFjI3NjQnMScHA4AlG/2AGyUlGwKAGyX9iA4ODigOdiUbGyV2DigODg76+gMAGiYmGhslJRv+jw4oDg4Odv6/GyUlGwFBdg4ODigO+voAAAACAIAAQAOAA0AADgApAAAlISIGFRQWMyEyNjU0JiMBFzc2NCcmIgcxBxE0JiMiBhURJyYiBwYUFzEDQP2AGyUlGwKAGyUlG/3I+vkODg4nDnYlGxsldg4oDg4OwCUbGyUlGxslATH6+g4oDg4OdgFBGyUlG/6/dg4ODigOAAAAAAEBDQDAAvMCuAAGAAABJwczETMRAvPz87OAAcD4+P8AAQAAAAQARgAGA7oDegAHAB4AIwArAAABJgYHDgEXAQUuAycBHgMXHgMXAS4DJwMBNwEHJQEWNjc+AScBiEecRxcKCQFCAXMfQ0dKJv58Cx8qNB8fQ0dKJgGECx8qNB9+/tA2ASowAT3+vkyXRxcKCQN6CQoXTJdHAUK5HzQqHwv+cCZKRkMgHzQpIAsBkCZKR0Mf/kwBKzv+1jw7/r4JChdMl0cAAAAAAQAAAAEAAIB4C1lfDzz1AAsEAAAAAADSoFUWAAAAANKgVRYAAP+rBBEDwAAAAAgAAgAAAAAAAAABAAADwP/AAAAEVQAAAAAEEQABAAAAAAAAAAAAAAAAAAAAPwQAAAAAAAAAAAAAAAIAAAAEAABABAAAAAQAAAAEAADABAAAwAQAAP4EAAD+BAAAwQQAAIAEAAAABAAAAAQAAAwEAACWBAAAAAQAAF8EAACZBAAAAAQAANsEAABUBAAAIAQAAIAEAACZBAAA5gQAAVUEAACWBAAAQAQAANUEAADEBAAAGwQAAFYEAAAbBAAAVgQAAAAEAACsBAAAAAQAAAAEAAAABAAAdAQAAMIEAADeBAABQAQAAAAEAAD/BAAA3gQAATkEAAAABAAAAAQAAFYEAADEBAAAAARVAEQEAABAA5QAIAQAAFEEAACXBAAAgAQAAIAEAAENBAAARgAAAAAACgAUAB4ATADQAVQBbgF+AZQBrAHgAiQCaAKYAsQDGANeA5wDsAPuBA4EcAVaBYgFnAW0BcwF7gYmBjwGVAZuBogGoAa4BuoHFAcyB2QHjgemB7wHzAfcCA4IHgguCD4IcAiSCLAI6AkQCbYKiArGCxwLTAuMC8wL3gwwAAEAAAA/ALQACwAAAAAAAgAAAAAAAAAAAAAAAAAAAAAAAAAOAK4AAQAAAAAAAQAOAAAAAQAAAAAAAgAHAJ8AAQAAAAAAAwAOAEsAAQAAAAAABAAOALQAAQAAAAAABQALACoAAQAAAAAABgAOAHUAAQAAAAAACgAaAN4AAwABBAkAAQAcAA4AAwABBAkAAgAOAKYAAwABBAkAAwAcAFkAAwABBAkABAAcAMIAAwABBAkABQAWADUAAwABBAkABgAcAIMAAwABBAkACgA0APhkZGctc2VycC1pY29ucwBkAGQAZwAtAHMAZQByAHAALQBpAGMAbwBuAHNWZXJzaW9uIDEuMABWAGUAcgBzAGkAbwBuACAAMQAuADBkZGctc2VycC1pY29ucwBkAGQAZwAtAHMAZQByAHAALQBpAGMAbwBuAHNkZGctc2VycC1pY29ucwBkAGQAZwAtAHMAZQByAHAALQBpAGMAbwBuAHNSZWd1bGFyAFIAZQBnAHUAbABhAHJkZGctc2VycC1pY29ucwBkAGQAZwAtAHMAZQByAHAALQBpAGMAbwBuAHNGb250IGdlbmVyYXRlZCBieSBJY29Nb29uLgBGAG8AbgB0ACAAZwBlAG4AZQByAGEAdABlAGQAIABiAHkAIABJAGMAbwBNAG8AbwBuAC4AAAADAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA") format("woff"), url("/font/ddg-serp-icons.ttf?v=120") format("truetype");
        font-weight: normal;
        font-style: normal
    }}
}}

.frm__input__clear,
.ddgsi,
.ddgsi-b:before,
.ddgsi-a:after {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale
    }}
}}

.ddgsi-alert:before {{
    {{
        content: "\21"
    }}
}}

.ddgsi-arrow-down:before {{
    {{
        content: "\2193"
    }}
}}

.ddgsi-arrow-left:before {{
    {{
        content: "\2190"
    }}
}}

.ddgsi-arrow-right:before {{
    {{
        content: "\2192"
    }}
}}

.ddgsi-arrow-top:before {{
    {{
        content: "\2b06"
    }}
}}

.ddgsi-arrow-up:before {{
    {{
        content: "\2191"
    }}
}}

.ddgsi-check:before {{
    {{
        content: "\2713"
    }}
}}

.ddgsi-check-sign:before {{
    {{
        content: "\2611"
    }}
}}

.ddgsi-circle:before {{
    {{
        content: "\25cf"
    }}
}}

.ddgsi-clock:before {{
    {{
        content: "\43"
    }}
}}

.ddgsi-close:before {{
    {{
        content: "\58"
    }}
}}

.ddgsi-close-bold:before {{
    {{
        content: "\d7"
    }}
}}

.ddgsi-cloudsave:before {{
    {{
        content: "\63"
    }}
}}

.ddgsi-comment:before {{
    {{
        content: "\4F"
    }}
}}

.ddgsi-cry:before {{
    {{
        content: "\2639"
    }}
}}

.ddgsi-directions:before {{
    {{
        content: "\64"
    }}
}}

.ddgsi-down:before {{
    {{
        content: "\76"
    }}
}}

.ddgsi-download:before {{
    {{
        content: "\2913"
    }}
}}

.ddgsi-eye:before {{
    {{
        content: "\69"
    }}
}}

.ddgsi-football:before {{
    {{
        content: "\1f3c8"
    }}
}}

.ddgsi-frown:before {{
    {{
        content: "\28"
    }}
}}

.ddgsi-grid:before {{
    {{
        content: "\47"
    }}
}}

.ddgsi-heart:before {{
    {{
        content: "\2665"
    }}
}}

.ddgsi-home:before {{
    {{
        content: "\2302"
    }}
}}

.ddgsi-info:before {{
    {{
        content: "\49"
    }}
}}

.ddgsi-left:before {{
    {{
        content: "\3c"
    }}
}}

.ddgsi-left-big:before {{
    {{
        content: "\2039"
    }}
}}

.ddgsi-left-sign:before {{
    {{
        content: "\25c1"
    }}
}}

.ddgsi-less-sign:before {{
    {{
        content: "\229d"
    }}
}}

.ddgsi-loupe:before {{
    {{
        content: "\53"
    }}
}}

.ddgsi-marker:before {{
    {{
        content: "\40"
    }}
}}

.ddgsi-menu:before {{
    {{
        content: "\21f6"
    }}
}}

.ddgsi-minus:before {{
    {{
        content: "\2d"
    }}
}}

.ddgsi-more:before {{
    {{
        content: "\2026"
    }}
}}

.ddgsi-more-sign:before {{
    {{
        content: "\2295"
    }}
}}

.ddgsi-move:before {{
    {{
        content: "\2629"
    }}
}}

.ddgsi-music:before {{
    {{
        content: "\266b"
    }}
}}

.ddgsi-news:before {{
    {{
        content: "\6e"
    }}
}}

.ddgsi-next:before {{
    {{
        content: "\bb"
    }}
}}

.ddgsi-pause:before {{
    {{
        content: "\2551"
    }}
}}

.ddgsi-play:before {{
    {{
        content: "\25ba"
    }}
}}

.ddgsi-plus:before {{
    {{
        content: "\2b"
    }}
}}

.ddgsi-prev:before {{
    {{
        content: "\ab"
    }}
}}

.ddgsi-region:before {{
    {{
        content: "\2637"
    }}
}}

.ddgsi-right:before {{
    {{
        content: "\3e"
    }}
}}

.ddgsi-right-big:before {{
    {{
        content: "\203a"
    }}
}}

.ddgsi-right-sign:before {{
    {{
        content: "\25b7"
    }}
}}

.ddgsi-smile:before {{
    {{
        content: "\29"
    }}
}}

.ddgsi-star:before {{
    {{
        content: "\2605"
    }}
}}

.ddgsi-swap:before {{
    {{
        content: "\21a9"
    }}
}}

.ddgsi-t-down:before {{
    {{
        content: "\25bc"
    }}
}}

.ddgsi-t-left:before {{
    {{
        content: "\25c0"
    }}
}}

.ddgsi-t-right:before {{
    {{
        content: "\25b6"
    }}
}}

.ddgsi-t-up:before {{
    {{
        content: "\25b2"
    }}
}}

.ddgsi-up:before {{
    {{
        content: "\5e"
    }}
}}

.ddgsi-uploaded:before {{
    {{
        content: "\21d1"
    }}
}}

.ddgsi-upload:before {{
    {{
        content: "\2912"
    }}
}}

.ddgsi-user:before {{
    {{
        content: "\75"
    }}
}}

.ddgsi-users:before {{
    {{
        content: "\55"
    }}
}}

.sticky {{
    {{
        position: -webkit-sticky;
        position: -moz-sticky;
        position: -ms-sticky;
        position: -o-sticky;
        position: sticky
    }}
}}

.sticky--dummy {{
    {{
        display: none
    }}
}}

.is-stuck+.sticky--dummy {{
    {{
        display: block
    }}
}}

.browser--chrome,
.browser--firefox,
.browser--safari,
.browser--opera,
.browser--ie,
.browser--edge,
.browser--arora,
.browser--maxthon,
.browser--seamonkey,
.browser--palemoon,
.browser--vivaldi,
.browser--ddg,
.logo_modal {{
    {{
        width: 64px;
        height: 64px;
        text-indent: -999999px;
        display: inline-block;
        vertical-align: middle;
        position: relative;
        background-position: 50% 50%;
        background-repeat: no-repeat
    }}
}}

.browser--chrome {{
    {{
        background-image: url("/assets/icons/browsers/small/chrome.png")
    }}
}}

.svg .browser--chrome {{
    {{
        background-image: url("/assets/icons/browsers/small/chrome.svg")
    }}
}}

.browser--firefox {{
    {{
        background-image: url("/assets/icons/browsers/small/firefox.png")
    }}
}}

.svg .browser--firefox {{
    {{
        background-image: url("/assets/icons/browsers/small/firefox.svg")
    }}
}}

.browser--safari {{
    {{
        background-image: url("/assets/icons/browsers/small/safari.png")
    }}
}}

.svg .browser--safari {{
    {{
        background-image: url("/assets/icons/browsers/small/safari.svg")
    }}
}}

.browser--opera {{
    {{
        background-image: url("/assets/icons/browsers/small/opera.png")
    }}
}}

.svg .browser--opera {{
    {{
        background-image: url("/assets/icons/browsers/small/opera.svg")
    }}
}}

.browser--ie {{
    {{
        background-image: url("/assets/icons/browsers/small/ie.png")
    }}
}}

.svg .browser--ie {{
    {{
        background-image: url("/assets/icons/browsers/small/ie.svg")
    }}
}}

.browser--edge {{
    {{
        background-image: url("/assets/icons/browsers/small/edge.png")
    }}
}}

.svg .browser--edge {{
    {{
        background-image: url("/assets/icons/browsers/small/edge.svg")
    }}
}}

.browser--arora {{
    {{
        background-image: url("/assets/icons/browsers/small/arora.png")
    }}
}}

.svg .browser--arora {{
    {{
        background-image: url("/assets/icons/browsers/small/arora.svg")
    }}
}}

.browser--maxthon {{
    {{
        background-image: url("/assets/icons/browsers/small/maxthon.png")
    }}
}}

.svg .browser--maxthon {{
    {{
        background-image: url("/assets/icons/browsers/small/maxthon.svg")
    }}
}}

.browser--seamonkey {{
    {{
        background-image: url("/assets/icons/browsers/small/seamonkey.png")
    }}
}}

.svg .browser--seamonkey {{
    {{
        background-image: url("/assets/icons/browsers/small/seamonkey.svg")
    }}
}}

.browser--palemoon {{
    {{
        background-image: url("/assets/icons/browsers/small/palemoon.png")
    }}
}}

.svg .browser--palemoon {{
    {{
        background-image: url("/assets/icons/browsers/small/palemoon.svg")
    }}
}}

.browser--vivaldi {{
    {{
        background-image: url("/assets/icons/browsers/small/vivaldi.png")
    }}
}}

.svg .browser--vivaldi {{
    {{
        background-image: url("/assets/icons/browsers/small/vivaldi.svg")
    }}
}}

.browser--ddg,
.logo_modal {{
    {{
        background-image: url("/assets/icons/browsers/small/ddg.png")
    }}
}}

.svg .browser--ddg,
.svg .logo_modal {{
    {{
        background-image: url("/assets/icons/browsers/small/ddg.svg")
    }}
}}

.logo_homepage,
.header__logo,
.logo--dax {{
    {{
        background-position: 50% 50%;
        background-repeat: no-repeat;
        margin: auto;
        display: block;
        position: relative;
        text-align: center;
        color: transparent;
        text-indent: -9999px;
        font-size: 0px
    }}
}}

.logo_homepage {{
    {{
        background-image: url("assets/logo_homepage.normal.v107.png");
        background-size: 250px 200px;
        width: 250px;
        height: 200px
    }}
}}

.svg .logo_homepage {{
    {{
        background-image: url("assets/logo_homepage.normal.v107.svg")
    }}
}}

.dark-bg .logo_homepage {{
    {{
        background-image: url("assets/logo_homepage.alt.v105.png")
    }}
}}

.svg.dark-bg .logo_homepage {{
    {{
        background-image: url("assets/logo_homepage.alt.v106.svg")
    }}
}}

@media only screen and (max-width: 425px) {{
    {{
        .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_small.normal.v107.png");
                background-size: 166px 130px;
                width: 166px;
                height: 130px
            }}
        }}
        .svg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_small.normal.v107.svg")
            }}
        }}
        .dark-bg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_small.alt.v105.png")
            }}
        }}
        .svg.dark-bg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_small.alt.v107.svg")
            }}
        }}
    }}
}}

@media only screen and (max-height: 382.5px) and (max-width: 590px),
only screen and (max-height: 361.25px) {{
    {{
        .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_mobile.normal.v107.png");
                background-size: 275px 62px;
                width: 275px;
                height: 62px
            }}
        }}
        .svg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_mobile.normal.v107.svg")
            }}
        }}
        .dark-bg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_mobile.alt.v105.png")
            }}
        }}
        .svg.dark-bg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_mobile.alt.v106.svg")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    {{
        .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage.normal.v107.retina.png")
            }}
        }}
        .dark-bg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage.alt.v105.retina.png")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min-resolution: 192dppx) and (max-width: 425px) {{
    {{
        .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_small.normal.v107.retina.png")
            }}
        }}
        .dark-bg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_small.alt.v105.retina.png")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min-resolution: 192dppx) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-webkit-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min-resolution: 192dppx) and (max-height: 361.25px) {{
    {{
        .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_mobile.normal.v107.retina.png")
            }}
        }}
        .dark-bg .logo_homepage {{
            {{
                background-image: url("assets/logo_homepage_mobile.alt.v105.retina.png")
            }}
        }}
    }}
}}

.header__logo {{
    {{
        background-image: url(assets/logo_header.v107.min.png);
        background-size: 50px 50px
    }}
}}

.svg .header__logo {{
    {{
        background-image: url(assets/logo_header.v107.min.svg)
    }}
}}

.dark-header .header__logo {{
    {{
        background-image: url(assets/logo_header_alt.v103.min.png)
    }}
}}

.svg.dark-header .header__logo {{
    {{
        background-image: url(assets/logo_header_alt.v103.min.svg)
    }}
}}

@media only screen and (min-width: 1079px) {{
    {{
        .header__logo {{
            {{
                background-image: url(assets/logo_header.v107.lg.png);
                background-size: 60px 60px
            }}
        }}
        .svg .header__logo {{
            {{
                background-image: url(assets/logo_header.v107.lg.svg)
            }}
        }}
        .dark-header .header__logo {{
            {{
                background-image: url(assets/logo_header_alt.v103.lg.png)
            }}
        }}
        .svg.dark-header .header__logo {{
            {{
                background-image: url(assets/logo_header_alt.v103.lg.svg)
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    {{
        .header__logo {{
            {{
                background-image: url(assets/logo_header.v107.min.retina.png)
            }}
        }}
        .dark-header .header__logo {{
            {{
                background-image: url(assets/logo_header_alt.v103.min.retina.png)
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2) and (min-width: 1079px),
only screen and (-moz-min-device-pixel-ratio: 2) and (min-width: 1079px),
only screen and (min--moz-device-pixel-ratio: 2) and (min-width: 1079px),
only screen and (-ms-min-device-pixel-ratio: 2) and (min-width: 1079px),
only screen and (min-device-pixel-ratio: 2) and (min-width: 1079px),
only screen and (min-resolution: 192dppx) and (min-width: 1079px) {{
    {{
        .header__logo {{
            {{
                background-image: url(assets/logo_header.v107.lg.retina.png)
            }}
        }}
        .dark-header .header__logo {{
            {{
                background-image: url(assets/logo_header_alt.v107.lg.retina.png)
            }}
        }}
    }}
}}

.logo--dax {{
    {{
        background-image: url("assets/dax.png");
        background-size: 90px 90px;
        width: 90px;
        height: 90px
    }}
}}

.svg .logo--dax {{
    {{
        background-image: url("assets/dax.svg")
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    {{
        .logo--dax {{
            {{
                background-image: url("assets/dax.retina.png")
            }}
        }}
    }}
}}

.logo_homepage__tt {{
    {{
        padding: 0 12px;
        font-weight: 600;
        font-size: 16px;
        bottom: auto;
        right: auto;
        left: 80%;
        top: 35%
    }}
}}

@media only screen and (max-height: 382.5px) and (max-width: 590px),
only screen and (max-height: 361.25px) {{
    {{
        .logo_homepage__tt {{
            {{
                bottom: 0;
                left: 95%;
                top: 0
            }}
        }}
    }}
}}

.no-touch .logo_homepage:hover .logo_homepage__tt {{
    {{
        -moz-transition-delay: 0.5s;
        -o-transition-delay: 0.5s;
        -webkit-transition-delay: 0.5s;
        transition-delay: 0.5s;
        visibility: visible;
        opacity: 1
    }}
}}

.search,
.search__input,
.search__clear,
.search__button,
.search--adv,
.search__input--adv {{
    {{
        font-size: 1em
    }}
}}

.search,
.search--adv {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        display: block;
        position: relative;
        height: 2.8em;
        background-color: #fff;
        border: 1px solid #d0d0d0;
        padding-left: 0.75em;
        padding-right: 6.5em
    }}
}}

.search:active:focus,
.search:active:hover,
.search--adv:active:focus,
.search--adv:active:hover {{
    {{
        border-color: rgba(208, 208, 208, 0.5)
    }}
}}

.search--adv {{
    {{
        padding-right: 3.5em
    }}
}}

.search--adv:hover,
.search--adv.search--header.has-text.search--hover,
.search--adv.search--header.has-text.search--focus,
.search--adv.search--home.has-text {{
    {{
        padding-right: 6.5em
    }}
}}

.search--no-clear {{
    {{
        padding-right: 3.5em
    }}
}}

.search--header {{
    {{
        background-color: #f7f7f7;
        height: 38px;
        padding-left: 9px
    }}
}}

.search--header .search__input,
.search--header .search__input--adv {{
    {{
        height: 38px
    }}
}}

.search--header .search__clear {{
    {{
        font-size: 14.4px
    }}
}}

.search--header .search__button {{
    {{
        font-size: 18px
    }}
}}

.lt-ie9 .search--header .search__input,
.lt-ie9 .search--header .search__input--adv {{
    {{
        line-height: 38px
    }}
}}

.search--home {{
    {{
        font-size: 1.14em
    }}
}}

.search--home .search__clear {{
    {{
        margin-right: 3.2em
    }}
}}

.lt-ie9 .search--home .search__button {{
    {{
        right: 7px
    }}
}}

.search--hero {{
    {{
        -moz-border-radius: 5px;
        -webkit-border-radius: 5px;
        border-radius: 5px;
        border-color: white;
        background-color: #e37151
    }}
}}

.search--hero .search__clear,
.search--hero .search__button,
.search--hero .search__button--hero {{
    {{
        color: white
    }}
}}

.search--hero .search__input,
.search--hero .search__input--adv {{
    {{
        font-weight: 300;
        color: white
    }}
}}

.search--hero .search__input::-moz-placeholder,
.search--hero .search__input--adv::-moz-placeholder {{
    {{
        color: white;
        opacity: 1
    }}
}}

.search--hero .search__input::-webkit-input-placeholder,
.search--hero .search__input--adv::-webkit-input-placeholder {{
    {{
        color: white
    }}
}}

.search__input,
.search__input--adv {{
    {{
        -webkit-appearance: none;
        -moz-appearance: none;
        -ms-appearance: none;
        -o-appearance: none;
        appearance: none;
        -webkit-tap-highlight-color: transparent;
        font-size: 1.1em;
        font-family: "DDG_ProximaNova", "DDG_ProximaNova_UI_0", "DDG_ProximaNova_UI_1", "DDG_ProximaNova_UI_2", "DDG_ProximaNova_UI_3", "DDG_ProximaNova_UI_4", "DDG_ProximaNova_UI_5", "DDG_ProximaNova_UI_6", "Proxima Nova", "Helvetica Neue", "Helvetica", "Segoe UI", "Nimbus Sans L", "Liberation Sans", "Open Sans", FreeSans, Arial, sans-serif;
        font-weight: normal;
        color: #333;
        display: block;
        width: 100%;
        background: none;
        outline: none;
        border: none;
        padding: 0;
        height: 2.54545em;
        z-index: 1;
        position: relative;
        top: -1px
    }}
}}

.search__input:focus,
.search__input--adv:focus {{
    {{
        outline: none
    }}
}}

.search__input::-ms-clear,
.search__input--adv::-ms-clear {{
    {{
        display: none
    }}
}}

.search__input::-moz-placeholder,
.search__input--adv::-moz-placeholder {{
    {{
        color: #aaa
    }}
}}

.search__input::-webkit-input-placeholder,
.search__input--adv::-webkit-input-placeholder {{
    {{
        color: #aaa
    }}
}}

.lt-ie9 .search__input,
.lt-ie9 .search__input--adv {{
    {{
        line-height: 2.54545em;
        font-family: "DDG_ProximaNova", "DDG_ProximaNova_UI_0", "DDG_ProximaNova_UI_1", "DDG_ProximaNova_UI_2", "DDG_ProximaNova_UI_3", "DDG_ProximaNova_UI_4", "DDG_ProximaNova_UI_5", "DDG_ProximaNova_UI_6", "Proxima Nova", "Helvetica Neue", "Helvetica", "Segoe UI", "Nimbus Sans L", "Liberation Sans", "Open Sans", FreeSans, Arial, sans-serif
    }}
}}

.search--focus .search__input--adv {{
    {{
        opacity: 1
    }}
}}

.search__hidden {{
    {{
        display: none
    }}
}}

.search__clear,
.search__button,
.search__button--hero {{
    {{
        -webkit-appearance: none;
        -moz-appearance: none;
        -ms-appearance: none;
        -o-appearance: none;
        appearance: none;
        -moz-box-sizing: content-box;
        -webkit-box-sizing: content-box;
        box-sizing: content-box;
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        -webkit-tap-highlight-color: transparent;
        width: 1em;
        display: block;
        cursor: pointer;
        background: transparent;
        text-align: center;
        border: none;
        height: 2.45em;
        line-height: 2.45em;
        position: absolute;
        top: 0;
        bottom: 0;
        right: 2px;
        left: auto;
        margin: auto;
        z-index: 2;
        outline: none
    }}
}}

.search__clear:active,
.search__button:active,
.search__button--hero:active {{
    {{
        -moz-transition: none 0.3s ease-in-out 0s;
        -o-transition: none 0.3s ease-in-out 0s;
        -webkit-transition: none 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: none 0.3s ease-in-out 0s
    }}
}}

.lt-ie9 .search__clear,
.lt-ie9 .search__button,
.lt-ie9 .search__button--hero {{
    {{
        right: 10px
    }}
}}

.lt-ie8 .search__clear,
.lt-ie8 .search__button,
.lt-ie8 .search__button--hero {{
    {{
        height: auto;
        border: none !important
    }}
}}

.search__dropdown {{
    {{
        display: none
    }}
}}

.search__clear {{
    {{
        padding: 0 0.5em;
        line-height: 1;
        min-width: 21px;
        margin-right: 3.6em;
        color: #aaa;
        visibility: hidden;
        opacity: 0
    }}
}}

.search__clear:focus,
.search__clear:hover {{
    {{
        outline: none;
        color: #333;
        visibility: visible
    }}
}}

.search__clear:active:focus,
.search__clear:active:hover {{
    {{
        color: #de5833
    }}
}}

.search__clear.empty,
.search--no-clear .search__clear {{
    {{
        display: none
    }}
}}

.search:hover .search__clear,
.search__input:focus ~ .search__clear,
.search--header.has-text.search--hover .search__clear,
.search--header.has-text.search--focus .search__clear,
.search--home.has-text .search__clear {{
    {{
        visibility: visible;
        opacity: 0.9
    }}
}}

.lt-ie8 .search__clear {{
    {{
        margin-right: 4.6em
    }}
}}

.is-mobile .search__clear {{
    {{
        text-align: right
    }}
}}

.search__button,
.search__button--hero {{
    {{
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        min-width: 26px;
        color: #aaa;
        font-size: 1.25em;
        padding: 0 0.64em;
        height: auto;
        min-height: 1.8em;
        margin-top: 2px;
        margin-bottom: 2px;
        line-height: 1.5;
        background-color: transparent;
        background-position: 50% 50%;
        background-repeat: no-repeat;
        -webkit-font-smoothing: subpixel-antialiased
    }}
}}

.search__button::-moz-focus-inner,
.search__button--hero::-moz-focus-inner {{
    {{
        margin-top: -1px
    }}
}}

.search__button:hover,
.search__button--hero:hover,
.search__button:focus,
.search__button--hero:focus {{
    {{
        outline: none
    }}
}}

.lt-ie8 .search__button,
.lt-ie8 .search__button--hero {{
    {{
        height: 1.96em;
        min-width: 3em
    }}
}}

.search:hover .search__button,
.search__input:focus ~ .search__button,
.search--header.has-text.search--hover .search__button,
.search--header.has-text.search--focus .search__button,
.search--home.has-text .search__button {{
    {{
        background-color: #5b9e4d;
        color: white
    }}
}}

.search__button:hover,
.search__button:focus,
.search:hover .search__button:focus,
.search--header.has-text.search--hover .search__button:hover,
.search--header.has-text.search--focus .search__button:hover,
.search--home.has-text .search__button:focus,
.search--home.has-text .search__button:hover {{
    {{
        background-color: #66ad57;
        color: white
    }}
}}

.search__button:active,
.search:hover .search__button:active,
.search--header.has-text.search--hover .search__button:active,
.search--home.has-text .search__button:active {{
    {{
        background-color: #333
    }}
}}

.search__button--active {{
    {{
        background-color: #5b9e4d;
        color: #fff
    }}
}}

.search__button--hero {{
    {{
        background-color: #e37151;
        color: white
    }}
}}

.badge-link {{
    {{
        -moz-border-radius: 4px;
        -webkit-border-radius: 4px;
        border-radius: 4px;
        background-color: #fff;
        border: 1px solid #d2d2d2;
        cursor: pointer;
        display: table;
        height: 64px;
        margin: 0 auto;
        position: relative;
        text-align: left
    }}
}}

.nav-menu--slideout .badge-link {{
    {{
        text-align: center;
        position: absolute;
        bottom: 2px;
        left: 0;
        margin: 0 8px 8px;
        padding: 5px
    }}
}}

.badge-link--top,
.badge-link--serp {{
    {{
        position: absolute;
        top: 60px;
        width: auto;
        height: auto;
        display: none
    }}
}}

.badge-link--top .badge-link__icon,
.badge-link--serp .badge-link__icon {{
    {{
        margin: 2px 5px
    }}
}}

.badge-link--top .badge-link__text,
.badge-link--serp .badge-link__text {{
    {{
        padding: 15px 40px 0 3px
    }}
}}

.badge-link--top {{
    {{
        right: 7px;
        z-index: 3
    }}
}}

.badge-link--serp {{
    {{
        -moz-box-shadow: 0 1px 6px rgba(0, 0, 0, 0.1);
        -webkit-box-shadow: 0 1px 6px rgba(0, 0, 0, 0.1);
        box-shadow: 0 1px 6px rgba(0, 0, 0, 0.1);
        border: 1px solid rgba(0, 0, 0, 0.12);
        right: 14px;
        z-index: 16
    }}
}}

@media only screen and (max-width: 979px) {{
    {{
        .badge-link--serp {{
            {{
                display: none !important
            }}
        }}
    }}
}}

.badge-link--bullets {{
    {{
        width: 280px
    }}
}}

.badge-link--bullets .badge-link__btn-group {{
    {{
        margin: 15px
    }}
}}

.badge-link--bullets .badge-link__btn-group .badge-link__btn {{
    {{
        padding-left: 0;
        padding-right: 0
    }}
}}

.badge-link__wrap {{
    {{
        display: block;
        position: relative
    }}
}}

.badge-link__wrap,
.badge-link__wrap:hover,
.badge-link__wrap:active,
.badge-link__wrap:visited {{
    {{
        text-decoration: none
    }}
}}

.badge-link__wrap--hidden {{
    {{
        display: none
    }}
}}

.badge-link__icon {{
    {{
        position: relative;
        float: left;
        display: block
    }}
}}

.nav-menu--slideout .badge-link__icon {{
    {{
        float: none;
        display: inline-block
    }}
}}

.badge-link__bullets {{
    {{
        display: block;
        clear: both;
        background: rgba(0, 0, 0, 0.04);
        border-top: 1px solid rgba(0, 0, 0, 0.1);
        border-bottom: 1px solid rgba(0, 0, 0, 0.1);
        font-size: 14px;
        color: #888;
        padding: 15px 15px 10px;
        line-height: 1.4
    }}
}}

.badge-link__bullets .badge-link__bullet {{
    {{
        margin-bottom: 10px
    }}
}}

.badge-link__bullets .badge-link__bullet-num {{
    {{
        display: block;
        margin: 0 10px 0 0;
        height: 24px;
        width: 10px;
        text-align: center;
        float: left
    }}
}}

.badge-link__btn {{
    {{
        display: block;
        margin: 1em
    }}
}}

.badge-link__btn-group {{
    {{
        clear: both;
        margin-bottom: 10px;
        margin: 0 40px 10px 73px
    }}
}}

.badge-link__btn-group .badge-link__btn {{
    {{
        display: inline-block;
        margin: 0;
        min-width: 100%
    }}
}}

.badge-link__title {{
    {{
        display: inline-block;
        font-size: 16px;
        font-weight: 600
    }}
}}

.badge-link__text {{
    {{
        color: #666;
        display: block;
        float: left;
        font-size: 14px;
        line-height: 20px;
        overflow: hidden;
        padding: 12px 36px 0 8px
    }}
}}

.nav-menu--slideout .badge-link__text {{
    {{
        padding: 0 8px
    }}
}}

.badge-link__cookie-msg {{
    {{
        color: #666;
        padding: 10px 15px;
        cursor: default
    }}
}}

.badge-link__close {{
    {{
        color: #888;
        cursor: pointer;
        display: block;
        font-size: 14px;
        padding: 8px;
        position: absolute;
        right: 0;
        top: 0
    }}
}}

.modal__box.modal__box--add-to-browser {{
    {{
        display: block;
        max-width: 316px;
        padding: 1.5em
    }}
}}

.add-to-browser__title {{
    {{
        font-size: 1.3em
    }}
}}

.add-to-browser__directions {{
    {{
        text-align: left;
        counter-reset: li;
        list-style: none
    }}
}}

.add-to-browser__directions>li {{
    {{
        padding: 0.25em 0.25em 0.5em 1.75em;
        position: relative;
        display: block;
        line-height: 1.2
    }}
}}

.add-to-browser__directions>li:before {{
    {{
        -moz-border-radius: 50%;
        -webkit-border-radius: 50%;
        border-radius: 50%;
        content: counter(li);
        counter-increment: li;
        background-color: #666;
        color: #fff;
        display: block;
        padding: 3px;
        position: absolute;
        margin-right: 0.5em;
        top: 3px;
        left: 0;
        text-align: center;
        font-size: 12px;
        line-height: 13px;
        height: 12px;
        width: 12px
    }}
}}

.add-to-browser__directions>li.has-btn {{
    {{
        line-height: 24px
    }}
}}

.add-to-browser__directions>li.has-btn:before {{
    {{
        top: 7px
    }}
}}

.add-to-browser__directions .btn--inline {{
    {{
        padding-left: 0.5em;
        padding-right: 0.5em
    }}
}}

.add-to-browser__image {{
    {{
        display: block;
        margin-top: 0.5em;
        max-width: none
    }}
}}

.add-to-browser__footer {{
    {{
        font-size: 0.9em;
        display: block;
        margin-top: 1.75em
    }}
}}

.add-to-browser__footer a:visited {{
    {{
        color: #4495d4
    }}
}}

.add-to-browser-badge--lite {{
    {{
        -moz-transition: opacity 0.3s ease-in-out 0s;
        -o-transition: opacity 0.3s ease-in-out 0s;
        -webkit-transition: opacity 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: opacity 0.3s ease-in-out 0s;
        opacity: 1
    }}
}}

.add-to-browser-badge--lite,
.add-to-browser-badge--lite:hover {{
    {{
        background: none;
        color: #666;
        border-color: #888
    }}
}}

.is-blurred .add-to-browser-badge--lite {{
    {{
        opacity: 0.5
    }}
}}

.has-search-focus .add-to-browser-badge--lite {{
    {{
        opacity: 0
    }}
}}

.add-to-browser--blurred {{
    {{
        -moz-transition: all 0.3s ease-in-out 0s;
        -o-transition: all 0.3s ease-in-out 0s;
        -webkit-transition: all 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: all 0.3s ease-in-out 0s;
        -moz-transform: translate3d(0px, 0px, 0px);
        -webkit-transform: translate3d(0px, 0px, 0px);
        transform: translate3d(0px, 0px, 0px);
        position: fixed;
        top: 100%;
        left: 0;
        width: 60%;
        text-align: center;
        padding: 15px 20% 140px;
        z-index: 300;
        font-size: 1.7em
    }}
}}

@media only screen and (max-height: 590px) {{
    {{
        .add-to-browser--blurred {{
            {{
                width: 96%;
                font-size: 1.2em;
                padding: 30px 2%
            }}
        }}
    }}
}}

.has-search-focus .add-to-browser--blurred {{
    {{
        display: none
    }}
}}

.add-to-browser--blurred__backdrop {{
    {{
        display: none;
        position: fixed;
        top: 0;
        left: 0;
        width: 0;
        height: 0
    }}
}}

.add-to-browser--blurred__backdrop.is-showing {{
    {{
        z-index: 299;
        display: block;
        width: 100%;
        height: 100%;
        background: rgba(255, 255, 255, 0.3)
    }}
}}

.blurred-animation .header-wrap,
.blurred-animation .zci-wrap,
.blurred-animation .content-wrap {{
    {{
        -moz-transition: all 0.3s ease-in-out 0s;
        -o-transition: all 0.3s ease-in-out 0s;
        -webkit-transition: all 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: all 0.3s ease-in-out 0s;
        -moz-transform: translate3d(0, 0, 0);
        -webkit-transform: translate3d(0, 0, 0);
        transform: translate3d(0, 0, 0);
        opacity: 1
    }}
}}

.is-blurred .header-wrap,
.is-blurred .zci-wrap,
.is-blurred .content-wrap {{
    {{
        opacity: 0.5
    }}
}}

.is-blurred.cssfilters .header-wrap,
.is-blurred.cssfilters .zci-wrap,
.is-blurred.cssfilters .content-wrap {{
    {{
        -webkit-filter: blur(5px);
        -moz-filter: blur(5px);
        filter: blur(5px);
        opacity: 1
    }}
}}

.atb-extension-overlay {{
    {{
        -moz-transition: opacity 0.3s ease-in-out 0s;
        -o-transition: opacity 0.3s ease-in-out 0s;
        -webkit-transition: opacity 0.3s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: opacity 0.3s ease-in-out 0s;
        position: fixed;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%;
        z-index: 3000;
        background: rgba(0, 0, 0, 0.75);
        color: #fff;
        opacity: 0
    }}
}}

.atb-extension-overlay.is-showing {{
    {{
        opacity: 1
    }}
}}

.atb-extension-overlay__content {{
    {{
        -moz-transition: opacity 0.1s ease-out 0.5s;
        -o-transition: opacity 0.1s ease-out 0.5s;
        -webkit-transition: opacity 0.1s ease-out;
        -webkit-transition-delay: 0.5s;
        transition: opacity 0.1s ease-out 0.5s;
        position: absolute;
        opacity: 0
    }}
}}

.is-showing .atb-extension-overlay__content {{
    {{
        opacity: 1
    }}
}}

.atb-extension-overlay--firefox .atb-extension-overlay__content {{
    {{
        top: 40px;
        left: 490px
    }}
}}

.is-osx.atb-extension-overlay--firefox .atb-extension-overlay__content {{
    {{
        left: 465px
    }}
}}

.atb-extension-overlay--safari .atb-extension-overlay__content {{
    {{
        position: absolute;
        margin: auto;
        bottom: 0;
        right: 0;
        left: 0;
        top: 0;
        width: 100%;
        height: 400px;
        text-align: center
    }}
}}

.atb-extension-overlay__header {{
    {{
        font-size: 2.8em;
        font-weight: bold;
        padding: 5px 0
    }}
}}

.atb-extension-overlay__left-col {{
    {{
        display: inline-block;
        text-align: right;
        width: 50%
    }}
}}

.atb-extension-overlay__right-col {{
    {{
        display: inline-block;
        text-align: left;
        width: 45%;
        margin-left: 5%
    }}
}}

.atb-extension-overlay__col-inner {{
    {{
        display: inline-block;
        width: 400px;
        text-align: left
    }}
}}

.atb-extension-overlay__icn {{
    {{
        font-size: 2em;
        padding-right: 10px;
        vertical-align: middle
    }}
}}

.atb-extension-overlay__img {{
    {{
        width: 280px
    }}
}}

.atb-extension-overlay__text {{
    {{
        font-size: 1.4em;
        vertical-align: middle;
        padding: 0
    }}
}}

.atb-extension-overlay--safari .atb-extension-overlay__text {{
    {{
        margin-bottom: 10px
    }}
}}

.atb-extension-overlay__subtext {{
    {{
        color: #aaa
    }}
}}

.atb-extension-overlay__link,
.atb-extension-overlay__link:active,
.atb-extension-overlay__link:hover,
.atb-extension-overlay__link:visited {{
    {{
        color: #aaa;
        text-decoration: underline
    }}
}}

.atb-extension-overlay__confetti {{
    {{
        position: absolute;
        top: 0;
        left: 0;
        width: 100%;
        height: 100%
    }}
}}

.atb-extension-overlay__success {{
    {{
        -moz-transition: top 0.5s ease-in;
        -o-transition: top 0.5s ease-in;
        -webkit-transition: top 0.5s ease-in;
        transition: top 0.5s ease-in;
        position: absolute;
        top: -100%;
        left: 0;
        right: 0;
        bottom: 0;
        width: 100%;
        height: 100%;
        text-align: center
    }}
}}

.atb-extension-overlay__success.is-showing {{
    {{
        top: 30%
    }}
}}

.atb-extension-overlay__success .btn--primary {{
    {{
        font-size: 1.5em;
        margin-top: 50px
    }}
}}

.atb-extension-overlay__success .atb-extension-overlay__text {{
    {{
        font-size: 2em
    }}
}}

.atb-extension-overlay__headline {{
    {{
        margin: 0;
        padding: 0;
        line-height: 1.2;
        font-size: 4em
    }}
}}

.tx-bld {{
    {{
        font-weight: 600
    }}
}}

.tx-lt,
.hd-hr,
.hd-lg {{
    {{
        font-weight: 300
    }}
}}

.tx-it {{
    {{
        font-weight: normal;
        font-style: italic
    }}
}}

.tx-up,
.hd-lg,
.hd-md {{
    {{
        text-transform: uppercase
    }}
}}

.hd-hr {{
    {{
        line-height: 1.1;
        text-align: center
    }}
}}

.hd-hr+.hd-hr {{
    {{
        padding-top: 0
    }}
}}

.hd-lg {{
    {{
        padding-top: 1em;
        padding-bottom: 0
    }}
}}

.hd-md .anchor--link {{
    {{
        float: right;
        font-size: 0.5em;
        margin-top: 1em
    }}
}}

.dropdown {{
    {{
        margin-right: 1em;
        display: inline-block;
        height: 33px;
        line-height: 33px;
        font-size: 1.1em;
        position: relative
    }}
}}

.dropdown__button {{
    {{
        color: #666;
        display: block;
        height: 100%;
        padding-right: 30px;
        line-height: 34px
    }}
}}

.dropdown__button:hover,
.dropdown__button:focus {{
    {{
        text-decoration: none;
        color: #666
    }}
}}

.dropdown__button:visited {{
    {{
        color: #666
    }}
}}

.dropdown__button:after {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        content: "\76";
        margin-top: -5px;
        font-size: 12px;
        line-height: 1;
        pointer-events: none;
        vertical-align: middle;
        display: inline-block;
        position: absolute;
        margin-left: 5px;
        top: 50%
    }}
}}

.modal--dropdown {{
    {{
        width: 100%;
        height: 0;
        position: absolute;
        left: auto;
        right: auto;
        top: auto;
        bottom: auto
    }}
}}

.modal--dropdown .modal__box {{
    {{
        margin-top: 0;
        width: auto;
        min-width: auto;
        text-align: left;
        left: -1em
    }}
}}

.modal--dropdown .modal__box:before,
.modal--dropdown .modal__box:after {{
    {{
        content: none
    }}
}}

.is-showing .modal--dropdown .modal__box {{
    {{
        z-index: 99
    }}
}}

.modal--dropdown .modal__body {{
    {{
        padding: 0.5em
    }}
}}

.modal--dropdown .modal__list__link {{
    {{
        white-space: nowrap;
        padding: 0.5em 0.75em
    }}
}}

.onboarding-pointer {{
    {{
        cursor: pointer
    }}
}}

.body--onboarding {{
    {{
        overflow-y: visible !important;
        position: static !important
    }}
}}

.onboarding-scroll-teaser {{
    {{
        text-align: center;
        position: absolute;
        bottom: 30px;
        width: 100%
    }}
}}

@media screen and (max-height: 550px) {{
    {{
        .onboarding-scroll-teaser {{
            {{
                display: none
            }}
        }}
    }}
}}

.onboarding-scroll-teaser .onboarding-scroll-teaser-arrow {{
    {{
        width: 24px;
        height: 28px;
        display: block;
        margin: 10px auto;
        cursor: pointer
    }}
}}

.onboarding-bottom {{
    {{
        width: 100%;
        position: absolute;
        top: 100%;
        height: 100%;
        min-height: 450px;
        overflow: hidden
    }}
}}

.onboarding-bottom .onboarding-bottom-close {{
    {{
        float: right;
        position: absolute;
        top: 20px;
        right: 30px;
        margin: 20px 30px;
        font-size: 20px;
        color: #bac9e4;
        cursor: pointer;
        z-index: 99;
        display: none
    }}
}}

.onboarding-bottom .onboarding-bottom-outer {{
    {{
        display: table;
        position: absolute;
        height: 100%;
        width: 100%;
        z-index: 10
    }}
}}

.onboarding-bottom .onboarding-bottom-middle {{
    {{
        display: table-cell;
        vertical-align: middle
    }}
}}

.onboarding-bottom .onboarding-bottom-inner {{
    {{
        margin-left: auto;
        margin-right: auto;
        max-width: 860px;
        padding-left: 20px
    }}
}}

.onboarding-bottom--astronaut {{
    {{
        background: #000 url(/assets/onboarding/stars.png) repeat top center !important;
        z-index: 0
    }}
}}

.onboarding-bottom--astronaut .onboarding-bottom-title-row {{
    {{
        margin-bottom: 0px;
        position: relative
    }}
}}

.onboarding-bottom--astronaut .onboarding-bottom-title-row .onboarding-bottom-title {{
    {{
        color: #fff;
        font-weight: bold;
        font-size: 34px;
        line-height: 1.2em;
        margin: 0;
        padding: 0;
        text-align: left;
        margin-bottom: 10px
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .onboarding-bottom--astronaut .onboarding-bottom-title-row .onboarding-bottom-title {{
            {{
                text-align: center
            }}
        }}
    }}
}}

.onboarding-bottom--astronaut .onboarding-bottom-title-row .onboarding-bottom-subtitle {{
    {{
        color: #bac9e4;
        text-align: left;
        font-size: 19px;
        margin: 0
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .onboarding-bottom--astronaut .onboarding-bottom-title-row .onboarding-bottom-subtitle {{
            {{
                text-align: center
            }}
        }}
    }}
}}

.onboarding-bottom--astronaut .onboarding-bottom-title-row .onboarding-bottom-bullets {{
    {{
        margin: 0px 0 20px 0;
        text-align: left
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .onboarding-bottom--astronaut .onboarding-bottom-title-row .onboarding-bottom-bullets {{
            {{
                text-align: center
            }}
        }}
    }}
}}

.onboarding-bottom--astronaut .onboarding-bottom-title-row .onboarding-bottom-bullet {{
    {{
        color: #efefef;
        font-size: 22px;
        line-height: 1em;
        margin: 15px 0 0px 0;
        padding: 0
    }}
}}

.onboarding-bottom--astronaut .onboarding-astronaut-img {{
    {{
        display: block;
        left: 520px;
        top: -120px;
        position: absolute
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .onboarding-bottom--astronaut .onboarding-astronaut-img {{
            {{
                display: none
            }}
        }}
    }}
}}

.onboarding-bottom--astronaut .onboarding-astronaut-img--lower {{
    {{
        left: 520px;
        top: -40px
    }}
}}

.onboarding-bottom--astronaut .onboarding-bottom-atb-row {{
    {{
        margin-top: 20px
    }}
}}

.onboarding-bottom--astronaut .onboarding-bottom-atb-row .btn {{
    {{
        position: static;
        display: inline-block !important;
        padding: 6px 25px !important;
        background: #60a5da;
        font-size: 20px;
        color: #fff
    }}
}}

.onboarding-bottom--astronaut .onboarding-bottom-atb-row .btn:hover {{
    {{
        background: #5898c9
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .onboarding-bottom--astronaut .onboarding-bottom-atb-row {{
            {{
                text-align: center
            }}
        }}
    }}
}}

@keyframes move-twink-back {{
    {{
        from {{
            {{
                background-position: 0 0
            }}
        }}
        to {{
            {{
                background-position: -10000px 5000px
            }}
        }}
    }}
}}

@-webkit-keyframes move-twink-back {{
    {{
        from {{
            {{
                background-position: 0 0
            }}
        }}
        to {{
            {{
                background-position: -10000px 5000px
            }}
        }}
    }}
}}

.onboarding-bottom-twinkling {{
    {{
        position: absolute;
        top: 0;
        left: 0;
        right: 0;
        bottom: 0;
        width: 100%;
        height: 100%;
        display: block;
        background: transparent url(/assets/onboarding/twinkling-new.png) repeat top center;
        z-index: 1;
        -webkit-animation: move-twink-back 200s linear infinite;
        animation: move-twink-back 200s linear infinite
    }}
}}

@-webkit-keyframes gentle-bounce-animation {{
    {{
        from {{
            {{
                transform: translateY(10px)
            }}
        }}
        to {{
            {{
                transform: translateY(-10px)
            }}
        }}
    }}
}}

@keyframes gentle-bounce-animation {{
    {{
        from {{
            {{
                transform: translateY(10px)
            }}
        }}
        to {{
            {{
                transform: translateY(-10px)
            }}
        }}
    }}
}}

.onboarding-bottom-astronaut .onboarding-astronaut-img {{
    {{
        animation: gentle-bounce-animation 3s infinite cubic-bezier(0.65, 0.05, 0.36, 1) alternate;
        -webkit-animation: gentle-bounce-animation 3s infinite cubic-bezier(0.65, 0.05, 0.36, 1) alternate
    }}
}}

.has-zcm .header {{
    {{
        width: 98.5%
    }}
}}

.header-wrap {{
    {{
        -moz-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
        -webkit-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
        box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
        -webkit-tab-highlight-color: transparent;
        background-color: #fff;
        padding-top: 16px;
        border-top: 2px solid #de5833;
        position: relative;
        z-index: 15
    }}
}}

.header-wrap:after {{
    {{
        content: "";
        width: 100%;
        display: block;
        margin-top: -2px;
        height: 1px
    }}
}}

.set-header--fixed.at-zci-bottom.has-active-zci .header-wrap {{
    {{
        -moz-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
        -webkit-box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15);
        box-shadow: 0 1px 0 rgba(0, 0, 0, 0.15)
    }}
}}

.lt-ie9 .header-wrap {{
    {{
        border-bottom: 1px solid #dbdbdb
    }}
}}

.header-wrap--show {{
    {{
        display: block
    }}
}}

.header-wrap--with-hero {{
    {{
        -moz-transition: top 0.1s ease-in-out 0s;
        -o-transition: top 0.1s ease-in-out 0s;
        -webkit-transition: top 0.1s ease-in-out;
        -webkit-transition-delay: 0s;
        transition: top 0.1s ease-in-out 0s;
        position: absolute;
        width: 100%;
        right: 0;
        left: 0;
        padding-bottom: 15px;
        top: -96px
    }}
}}

.has-search-focus .header-wrap--with-hero {{
    {{
        top: 0
    }}
}}

.header-wrap--fixed {{
    {{
        position: fixed;
        top: 0
    }}
}}

.header.cw,
.header.cw--c {{
    {{
        padding-left: 0;
        padding-right: 0
    }}
}}

@media only screen and (max-width: 864px) {{
    {{
        .has-zcm .header {{
            {{
                margin-bottom: 0;
                width: 98%
            }}
        }}
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .has-zcm .header {{
            {{
                float: none;
                width: auto
            }}
        }}
    }}
}}

.lt-ie9 .header {{
    {{
        float: none
    }}
}}

.header__search-wrap {{
    {{
        position: relative;
        margin-bottom: 3px;
        margin-top: 1px
    }}
}}

.header__logo-wrap {{
    {{
        height: 50px;
        margin: auto;
        margin-left: 3.5px
    }}
}}

.header__logo-wrap:hover {{
    {{
        text-decoration: none
    }}
}}

.header__logo {{
    {{
        display: block;
        height: 46px;
        width: 46px;
        margin: -4px auto auto
    }}
}}

@media only screen and (min-width: 1079px) {{
    {{
        .header__logo {{
            {{
                width: 60px;
                height: 60px;
                margin-top: -10px
            }}
        }}
    }}
}}

.header__search {{
    {{
        position: relative;
        left: 7px
    }}
}}

.header--aside {{
    {{
        display: block;
        margin: 18px auto auto;
        width: 36px;
        height: 36px;
        right: 7px;
        position: absolute;
        bottom: 0;
        top: 0
    }}
}}

.header__button,
.header__button--menu {{
    {{
        display: block;
        float: right;
        top: 1px;
        z-index: 10
    }}
}}

.header__button,
.header__button--menu,
.header__button:visited,
.header__button--menu:visited,
.header__button:focus,
.header__button--menu:focus,
.header__button:hover,
.header__button--menu:hover {{
    {{
        color: #a5a5a5
    }}
}}

.header--aside__msg {{
    {{
        display: none;
        white-space: nowrap;
        position: absolute;
        margin: auto;
        right: 48px;
        bottom: 0;
        top: 0;
        z-index: 1;
        color: #aaa;
        line-height: 36px;
        font-size: 13px
    }}
}}

@media only screen and (min-width: 1280px) {{
    {{
        .header--aside__msg {{
            {{
                display: block
            }}
        }}
    }}
}}

.header-wrap--hero {{
    {{
        background: #e37151;
        color: #fff;
        position: relative;
        width: 100%;
        right: 0;
        left: 0;
        top: 0;
        padding-top: 1.25em
    }}
}}

.header-wrap--hero--alt {{
    {{
        border-top-color: #3a7fb4;
        background-color: #60a5da
    }}
}}

.header--hero {{
    {{
        cursor: pointer;
        line-height: 1.9;
        z-index: 5
    }}
}}

.header--hero__loupe {{
    {{
        position: relative;
        vertical-align: middle;
        margin-left: 0.8em;
        margin-right: 0.5em;
        top: 0;
        font-size: 1.5em
    }}
}}

.header--hero__loupe:before {{
    {{
        font-family: 'ddg-serp-icons' !important;
        speak: none;
        font-style: normal;
        font-weight: normal !important;
        font-variant: normal;
        text-transform: none;
        text-decoration: none !important;
        -webkit-font-smoothing: antialiased;
        -moz-osx-font-smoothing: grayscale;
        content: "\53"
    }}
}}

.header--hero__link {{
    {{
        padding-left: 1em;
        font-size: 16px
    }}
}}

.header--hero__link,
.header--hero__link:hover,
.header--hero__link:focus,
.header--hero__link:active {{
    {{
        color: #fff
    }}
}}

.header-wrap--hero .header__button--menu,
.header-wrap--hero .header__button--menu:visited,
.header-wrap--hero .header__button--menu:focus,
.header-wrap--hero .header__button--menu:hover {{
    {{
        color: #fff
    }}
}}

.no-touch .header-wrap--hero .header__button--menu:hover,
.no-touch .header-wrap--hero .header__button--menu:focus,
.no-js .header-wrap--hero .header__button--menu:hover,
.no-js .header-wrap--hero .header__button--menu:focus {{
    {{
        background-color: #bd4b2b;
        background-color: rgba(0, 0, 0, 0.15)
    }}
}}

@media only screen and (max-width: 864px) {{
    {{
        .header__search-wrap {{
            {{
                padding-right: 40px
            }}
        }}
        .header--aside {{
            {{
                width: 40px
            }}
        }}
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        .header__search-wrap {{
            {{
                padding-right: 40px
            }}
        }}
        .header__logo-wrap {{
            {{
                margin-left: 0
            }}
        }}
        .header__search {{
            {{
                left: 0
            }}
        }}
        .header--aside {{
            {{
                right: 3.5px
            }}
        }}
        .has-search-focus .header__search-wrap {{
            {{
                margin-left: 0;
                padding-left: 7px;
                padding-right: 7px
            }}
        }}
        .has-search-focus .header-wrap .header--aside {{
            {{
                right: -40px
            }}
        }}
        .has-search-focus .header__logo {{
            {{
                margin-left: -50px;
                opacity: 0
            }}
        }}
        .header-wrap {{
            {{
                padding-top: 9px
            }}
        }}
        .header--aside {{
            {{
                margin-top: 11px
            }}
        }}
        .header-wrap--hero {{
            {{
                padding-top: 11px
            }}
        }}
    }}
}}

.search__autocomplete {{
    {{
        display: none;
        position: absolute;
        top: 2.8em;
        left: 0;
        width: 100%;
        max-height: 208px;
        border: 1px solid #d0d0d0;
        border-radius: 0 0 2px 2px;
        margin-top: -2px;
        margin-left: -1px;
        margin-right: -1px;
        z-index: 25;
        box-shadow: 0 2px 4px rgba(0, 0, 0, 0.1)
    }}
}}

.header__search .search__autocomplete {{
    {{
        top: 38px
    }}
}}

.lt-ie8 .search__autocomplete {{
    {{
        display: none !important
    }}
}}

.acp-wrap {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        display: block;
        padding: 4px 0;
        max-height: 208px;
        background-color: #fff;
        background-color: rgba(255, 255, 255, 0.95);
        overflow: auto;
        overflow-x: hidden;
        position: static
    }}
}}

.search--home .acp-wrap {{
    {{
        font-size: 0.9em
    }}
}}

.acp-wrap.acp-wrap--bang {{
    {{
        padding: 0
    }}
}}

.lt-ie9 .acp-wrap {{
    {{
        max-height: 200px
    }}
}}

.acp-footer {{
    {{
        position: absolute;
        width: 100%;
        box-shadow: 0 0px 4px rgba(0, 0, 0, 0.1);
        border: 1px solid #d0d0d0;
        background-color: #fff;
        margin-left: -1px;
        padding: 4px 0
    }}
}}

.acp-footer .acp-footer__instructions {{
    {{
        margin-left: 16px;
        color: #959595
    }}
}}

.acp-footer .acp-footer__link {{
    {{
        position: absolute;
        right: 0;
        margin-right: 16px
    }}
}}

.acp,
.acp--bang {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis;
        cursor: pointer;
        line-height: 1.1;
        font-size: 1.1em;
        padding: 4px 0.68182em;
        margin: 0 auto;
        position: static;
        color: #a5a5a5
    }}
}}

.acp strong,
.acp--bang strong {{
    {{
        color: #505050;
        font-weight: normal
    }}
}}

.acp-wrap__column {{
    {{
        -moz-box-sizing: border-box;
        -webkit-box-sizing: border-box;
        box-sizing: border-box;
        width: 50%;
        float: left;
        padding: 4px
    }}
}}

.acp-wrap__column.acp-wrap__column--left {{
    {{
        border-right: 1px solid #e0e0e0
    }}
}}

.acp--bang {{
    {{
        -moz-border-radius: 2px;
        -webkit-border-radius: 2px;
        border-radius: 2px;
        padding: 8px 16px;
        position: relative;
        z-index: 1
    }}
}}

.acp--bang__img-wrap {{
    {{
        margin-right: 4px;
        background-color: #fff;
        float: left;
        width: 16px;
        height: 16px
    }}
}}

.acp--bang__img-wrap,
.acp--bang__img {{
    {{
        -moz-border-radius: 3px;
        -webkit-border-radius: 3px;
        border-radius: 3px
    }}
}}

.acp--bang__body {{
    {{
        position: relative;
        padding-left: 12px
    }}
}}

.acp--bang__phrase,
.acp--bang__snippet {{
    {{
        white-space: nowrap;
        overflow: hidden;
        -ms-text-overflow: ellipsis;
        -o-text-overflow: ellipsis;
        text-overflow: ellipsis
    }}
}}

.acp--bang__phrase {{
    {{
        color: #4c4c4c;
        font-weight: bold;
        position: absolute;
        right: 0
    }}
}}

.acp--bang__snippet {{
    {{
        color: #959595;
        display: inline-block;
        width: 75%
    }}
}}

.acp--long-phrase .acp--bang__snippet {{
    {{
        width: 50%
    }}
}}

.acp--highlight {{
    {{
        background-color: #f2f2f2;
        padding-top: 8px;
        padding-bottom: 8px;
        margin-top: -4px;
        margin-bottom: -4px
    }}
}}

.acp--highlight.acp--bang {{
    {{
        margin-top: 0;
        margin-bottom: 0
    }}
}}

.acp--highlight.acp--bang .acp--bang__snippet {{
    {{
        color: #111
    }}
}}

.acp--highlight,
.acp--highlight strong {{
    {{
        color: #505050
    }}
}}

.is-mobile .acp-wrap__column.acp-wrap__column--left {{
    {{
        float: none;
        width: 100%;
        border-right: none
    }}
}}

.is-mobile .acp-wrap__column.acp-wrap__column--right {{
    {{
        display: none
    }}
}}

.is-mobile .acp-footer {{
    {{
        display: none
    }}
}}

@media only screen and (max-width: 864px) {{
    {{
        .acp--bang__snippet {{
            {{
                width: 50%
            }}
        }}
        .acp--long-phrase .acp--bang__snippet {{
            {{
                width: 30%
            }}
        }}
    }}
}}

.is-mobile .hide--mob {{
    {{
        display: none
    }}
}}

.is-mobile .show--mob {{
    {{
        display: block
    }}
}}

.set-wide .header__search-wrap,
.set-wide .msg--box,
.set-wide .msg--info,
.set-wide .msg--help,
.set-wide .msg--untranslated,
.set-wide .msg--warning,
.set-wide #error_homepage,
.set-wide .msg,
.set-wide .content__text,
.set-wide .results--sidebar--mid {{
    {{
        max-width: 864px
    }}
}}

.set-wide .cw,
.set-wide .cw--c {{
    {{
        max-width: 1282px
    }}
}}

.set-wide .header--aside__msg {{
    {{
        display: none
    }}
}}

.set-wide .header__search-wrap {{
    {{
        max-width: 590px
    }}
}}

.set-super-wide .header__search-wrap,
.set-super-wide .msg--box,
.set-super-wide .msg--info,
.set-super-wide .msg--help,
.set-super-wide .msg--untranslated,
.set-super-wide .msg--warning,
.set-super-wide #error_homepage,
.set-super-wide .msg,
.set-super-wide .content__text,
.set-super-wide .results--sidebar--mid {{
    {{
        max-width: 1152px
    }}
}}

.set-super-wide .cw,
.set-super-wide .cw--c {{
    {{
        max-width: 1483px
    }}
}}

.set-super-wide .header--aside__msg {{
    {{
        display: none
    }}
}}

.set-super-wide .header__search-wrap {{
    {{
        max-width: 590px
    }}
}}

.set-header--fixed body {{
    {{
        padding-top: 64px
    }}
}}

.set-header--fixed .anchor {{
    {{
        top: -96px
    }}
}}

.set-header--fixed .metabar--fixed {{
    {{
        top: 64px
    }}
}}

.set-header--fixed.has-zcm body {{
    {{
        padding-top: 101px
    }}
}}

.set-header--fixed.has-zcm .anchor {{
    {{
        top: -101px
    }}
}}

.set-header--fixed.has-zcm .metabar--fixed {{
    {{
        top: 97px
    }}
}}

.set-header--fixed.is-mobile.has-zcm body {{
    {{
        padding-top: 94px
    }}
}}

.set-header--fixed.is-mobile.has-zcm .metabar--fixed {{
    {{
        top: 93px
    }}
}}

.set-header--fixed .site-wrapper {{
    {{
        margin-top: -3px
    }}
}}

.set-header--fixed .header-wrap {{
    {{
        position: fixed;
        width: 100%;
        top: 0;
        left: 0;
        right: 0;
        z-index: 100;
        padding-bottom: 0
    }}
}}

.set-header--fixed.has-search-focus.is-mobile-device .header-wrap {{
    {{
        position: absolute
    }}
}}

.set-header--fixed .results--sidebar--mid {{
    {{
        display: none
    }}
}}

@media only screen and (max-height: 318.75px),
only screen and (max-height: 382.5px) and (min-width: 425px) {{
    {{
        .set-header--fixed .header-wrap {{
            {{
                position: absolute
            }}
        }}
        .set-header--fixed .metabar--fixed,
        .set-header--fixed.has-zcm .metabar--fixed {{
            {{
                top: 0
            }}
        }}
    }}
}}

.set-header--menu.has-zcm .header-wrap {{
    {{
        padding-top: 6px
    }}
}}

.set-header--menu.has-zcm .header__search-wrap {{
    {{
        display: none
    }}
}}

.set-header--menu.has-zcm .header--aside {{
    {{
        margin: 4px auto
    }}
}}

.set-header--menu.has-zcm.is-mobile .header--aside {{
    {{
        background: -webkit-linear-gradient(left, rgba(255, 255, 255, 0) 0%, #fff 40%, #fff 100%);
        background: linear-gradient(to right, rgba(255, 255, 255, 0) 0%, #ffffff 40%, #ffffff 100%);
        width: 4em;
        right: 0
    }}
}}

.set-header--menu.has-zcm.is-mobile .header__button--menu {{
    {{
        right: 4px
    }}
}}

.set-header--menu.has-zcm.is-app .header-wrap {{
    {{
        border-top: none;
        border-bottom: 0;
        padding-top: 0
    }}
}}

.set-header--menu.has-zcm.is-app .header--aside {{
    {{
        display: none
    }}
}}

.set-header--menu .results--gutter {{
    {{
        margin-top: 0.5em
    }}
}}

.set-header--menu.is-mobile-device .header-wrap {{
    {{
        border-top-color: transparent
    }}
}}

.set-header--menu.is-mobile-device .site-wrapper:before {{
    {{
        display: none
    }}
}}

.set-text--small {{
    {{
        font-size: 72%
    }}
}}

.set-text--medium {{
    {{
        font-size: 81%
    }}
}}

.set-text--larger {{
    {{
        font-size: 99%
    }}
}}

.set-text--largest {{
    {{
        font-size: 112.5%
    }}
}}

.set-align-center .cw,
.set-align-center .cw--c,
.set-align-center .cw--c {{
    {{
        margin-left: auto;
        float: none
    }}
}}

.set-align-center .cw .zci__main--detail,
.set-align-center .cw--c .zci__main--detail,
.set-align-center .cw--c .zci__main--detail {{
    {{
        padding-left: 0;
        margin: 0 auto
    }}
}}

@media only screen and (max-width: 864px) {{
    {{
        .set-align-center .cw.header,
        .set-align-center .header.cw--c,
        .set-align-center .cw--c.header {{
            {{
                margin-left: 0
            }}
        }}
    }}
}}

.set-align-center .cw.has-aux,
.set-align-center .has-aux.cw--c {{
    {{
        margin-left: 0
    }}
}}

@media only screen and (min-width: 864px) {{
    {{
        .set-align-center .cw.has-aux,
        .set-align-center .has-aux.cw--c {{
            {{
                width: 77%
            }}
        }}
    }}
}}

@media only screen and (min-width: 971.1px) {{
    {{
        .set-align-center .cw.has-aux,
        .set-align-center .has-aux.cw--c {{
            {{
                width: 67%
            }}
        }}
    }}
}}

.set-align-center .cw.has-aux .zci__main,
.set-align-center .has-aux.cw--c .zci__main {{
    {{
        padding-left: 0
    }}
}}

.set-align-center .results-wrapper,
.set-align-center .metabar__in,
.set-align-center .zci__main.has-tiles {{
    {{
        padding-left: 0
    }}
}}

.set-align-center .header__search-wrap,
.set-align-center .zcm-wrap--header,
.set-align-center .msg--result,
.set-align-center .msg--serp,
.set-align-center .results--ads,
.set-align-center .results {{
    {{
        margin-left: auto;
        margin-right: auto
    }}
}}

.set-align-center.has-right-rail-ads .results {{
    {{
        padding-right: 60px
    }}
}}

.set-align-center .zcm-wrap {{
    {{
        padding-left: 0
    }}
}}

.set-align-center .header--aside__msg {{
    {{
        display: none
    }}
}}

.set-align-center .results--sidebar--mid {{
    {{
        margin: auto;
        right: 0
    }}
}}

.set-align-center .results--sidebar--mid .btn--top {{
    {{
        right: 0
    }}
}}

.logo_homepage--it {{
    {{
        background-image: url("/assets/logos/initech/logo_homepage.png")
    }}
}}

.svg .logo_homepage--it {{
    {{
        background-image: url("/assets/logos/initech/logo_homepage.svg")
    }}
}}

@media only screen and (max-height: 382.5px) and (max-width: 590px),
only screen and (max-height: 361.25px) {{
    {{
        .logo_homepage--it {{
            {{
                background-image: url("/assets/logos/initech/logo_homepage_mobile.png")
            }}
        }}
        .svg .logo_homepage--it {{
            {{
                background-image: url("/assets/logos/initech/logo_homepage_mobile.svg")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    {{
        .logo_homepage--it {{
            {{
                background-image: url("/assets/logos/initech/logo_homepage.retina.png")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min-resolution: 192dppx) and (max-width: 425px) {{
    {{
        .logo_homepage--it {{
            {{
                background-image: url("/assets/logos/initech/logo_homepage_small.retina.png")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min-resolution: 192dppx) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-webkit-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min-resolution: 192dppx) and (max-height: 361.25px) {{
    {{
        .logo_homepage--it {{
            {{
                background-image: url("/assets/logos/initech/logo_homepage_mobile.retina.png")
            }}
        }}
    }}
}}

.logo_homepage--it.header__logo {{
    {{
        width: 60px
    }}
}}

.logo_homepage--resetthenet {{
    {{
        background-image: url("/assets/logos/resetthenet/logo_homepage.normal.png")
    }}
}}

.svg .logo_homepage--resetthenet {{
    {{
        background-image: url("/assets/logos/resetthenet/logo_homepage.normal.svg")
    }}
}}

.dark-bg .logo_homepage--resetthenet {{
    {{
        background-image: url("/assets/logos/resetthenet/logo_homepage.alt.png")
    }}
}}

.svg.dark-bg .logo_homepage--resetthenet {{
    {{
        background-image: url("/assets/logos/resetthenet/logo_homepage.alt.svg")
    }}
}}

@media only screen and (max-width: 425px) {{
    {{
        .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_small.normal.png")
            }}
        }}
        .svg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_small.normal.svg")
            }}
        }}
        .dark-bg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_small.alt.png")
            }}
        }}
        .svg.dark-bg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_small.alt.svg")
            }}
        }}
    }}
}}

@media only screen and (max-height: 382.5px) and (max-width: 590px),
only screen and (max-height: 361.25px) {{
    {{
        .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_mobile.normal.png")
            }}
        }}
        .svg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_mobile.normal.svg")
            }}
        }}
        .dark-bg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_mobile.alt.png")
            }}
        }}
        .svg.dark-bg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_mobile.alt.svg")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2),
only screen and (-moz-min-device-pixel-ratio: 2),
only screen and (min--moz-device-pixel-ratio: 2),
only screen and (-ms-min-device-pixel-ratio: 2),
only screen and (min-device-pixel-ratio: 2),
only screen and (min-resolution: 192dppx) {{
    {{
        .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage.normal.retina.png")
            }}
        }}
        .dark-bg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage.alt.retina.png")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min-device-pixel-ratio: 2) and (max-width: 425px),
only screen and (min-resolution: 192dppx) and (max-width: 425px) {{
    {{
        .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_small.normal.retina.png")
            }}
        }}
        .dark-bg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_small.alt.retina.png")
            }}
        }}
    }}
}}

@media only screen and (-webkit-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min-device-pixel-ratio: 2) and (max-height: 382.5px) and (max-width: 590px),
only screen and (min-resolution: 192dppx) and (max-height: 382.5px) and (max-width: 590px),
only screen and (-webkit-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (-moz-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min--moz-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (-ms-min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min-device-pixel-ratio: 2) and (max-height: 361.25px),
only screen and (min-resolution: 192dppx) and (max-height: 361.25px) {{
    {{
        .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_mobile.normal.retina.png")
            }}
        }}
        .dark-bg .logo_homepage--resetthenet {{
            {{
                background-image: url("/assets/logos/resetthenet/logo_homepage_mobile.alt.retina.png")
            }}
        }}
    }}
}}

.is-open {{
    {{
        display: block
    }}
}}

.is-closed,
.is-hidden {{
    {{
        display: none !important
    }}
}}

.no-js .no-js__hide {{
    {{
        display: none !important
    }}
}}

.cw,
.cw--c {{
    {{
        position: relative;
        padding: 0 7px;
        max-width: 1008px;
        margin-left: 0;
        margin-right: auto
    }}
}}

.cw--c {{
    {{
        margin-left: auto
    }}
}}

.content__internal {{
    {{
        padding-top: 1em
    }}
}}

img {{
    {{
        max-width: 100%
    }}
}}

.site-wrapper {{
    {{
        width: 100%;
        overflow: hidden
    }}
}}

.content-wrap {{
    {{
        margin: 0 auto;
        position: relative
    }}
}}

.text-center,
.text-mid {{
    {{
        text-align: center
    }}
}}

.text-left {{
    {{
        text-align: left
    }}
}}

.text-right {{
    {{
        text-align: right
    }}
}}

.r-block {{
    {{
        display: block
    }}
}}

.r-inline {{
    {{
        display: inline
    }}
}}

.r-inline-block,
.r-iblock {{
    {{
        display: inline-block
    }}
}}

.r-valign--top {{
    {{
        vertical-align: top
    }}
}}

.r-valign--mid {{
    {{
        vertical-align: middle
    }}
}}

.r-valign--bottom {{
    {{
        vertical-align: bottom
    }}
}}

.r-border-box,
html {{
    {{
        -webkit-box-sizing: border-box;
        -moz-box-sizing: border-box;
        -ms-box-sizing: border-box;
        -o-box-sizing: border-box;
        box-sizing: border-box
    }}
}}

.r-content-box,
.r-grid-pad {{
    {{
        -webkit-box-sizing: content-box;
        -moz-box-sizing: content-box;
        -ms-box-sizing: content-box;
        -o-box-sizing: content-box;
        box-sizing: content-box
    }}
}}

.whole,
.half,
.third,
.twothird,
.twothirds,
.quarter,
.threequarter,
.threequarters,
.sixth,
.eighth,
.ninety,
.eighty,
.seventy,
.sixty,
.fifty,
.forty,
.thirty,
.twenty,
.ten {{
    {{
        float: left;
        position: relative;
        -webkit-box-sizing: border-box;
        -moz-box-sizing: border-box;
        -ms-box-sizing: border-box;
        -o-box-sizing: border-box;
        box-sizing: border-box
    }}
}}

.whole {{
    {{
        width: 100%
    }}
}}

.half,
.fifty {{
    {{
        width: 50%
    }}
}}

.third {{
    {{
        width: 33.3%
    }}
}}

.twothird,
.twothirds {{
    {{
        width: 66.6%
    }}
}}

.quarter {{
    {{
        width: 25%
    }}
}}

.threequarter,
.threequarters {{
    {{
        width: 75%
    }}
}}

.sixth {{
    {{
        width: 16.6%
    }}
}}

.eighth {{
    {{
        width: 12.5%
    }}
}}

.ninety {{
    {{
        width: 90%
    }}
}}

.eighty {{
    {{
        width: 80%
    }}
}}

.seventy {{
    {{
        width: 70%
    }}
}}

.sixty {{
    {{
        width: 60%
    }}
}}

.forty {{
    {{
        width: 40%
    }}
}}

.thirty {{
    {{
        width: 30%
    }}
}}

.twenty {{
    {{
        width: 20%
    }}
}}

.ten {{
    {{
        width: 10%
    }}
}}

.gw {{
    {{
        margin-left: -1em;
        letter-spacing: -0.31em
    }}
}}

.g {{
    {{
        padding-left: 1em;
        display: inline-block;
        vertical-align: top;
        letter-spacing: normal;
        float: none;
        -webkit-box-sizing: border-box;
        -moz-box-sizing: border-box;
        -ms-box-sizing: border-box;
        -o-box-sizing: border-box;
        box-sizing: border-box
    }}
}}

.gw--h {{
    {{
        margin-left: -0.5em
    }}
}}

.gw--h .g {{
    {{
        padding-left: 0.5em
    }}
}}

.block-mid {{
    {{
        margin-left: auto;
        margin-right: auto;
        display: block;
        float: none
    }}
}}

.pull-left,
.fl,
.float--left {{
    {{
        float: left
    }}
}}

.pull-right,
.fr,
.float--right {{
    {{
        float: right
    }}
}}

.pull-none,
.killfloat {{
    {{
        float: none
    }}
}}

.clear,
.r-clear {{
    {{
        clear: both
    }}
}}

.hide,
.r-hide {{
    {{
        display: none
    }}
}}

.hide--important,
#state_hidden,
#iframe_hidden {{
    {{
        display: none !important
    }}
}}

.allcaps {{
    {{
        text-transform: uppercase
    }}
}}

.clearfix:after,
.fix:after,
.group:after,
.row:after,
.gw:after,
.media:after,
.acp--bang:after {{
    {{
        content: "";
        display: block;
        clear: both
    }}
}}

@media only screen and (min-width: 1079px) {{
    {{
        .hide--screen-l {{
            {{
                display: none
            }}
        }}
        .show--screen-l,
        .r-block--screen-l {{
            {{
                display: block
            }}
        }}
        .block-mid--screen-l {{
            {{
                margin-left: auto;
                margin-right: auto;
                display: block;
                float: none
            }}
        }}
        .pull-left--screen-l,
        .fl--screen-l {{
            {{
                float: left
            }}
        }}
        .pull-right--screen-l,
        .fr--screen-l {{
            {{
                float: right
            }}
        }}
        .pull-none--screen-l,
        .killfloat--screen-l {{
            {{
                float: none
            }}
        }}
        .whole--screen-l {{
            {{
                width: 100%
            }}
        }}
        .ninety--screen-l {{
            {{
                width: 90%
            }}
        }}
        .eighty--screen-l {{
            {{
                width: 80%
            }}
        }}
        .seventy--screen-l {{
            {{
                width: 70%
            }}
        }}
        .sixty--screen-l {{
            {{
                width: 60%
            }}
        }}
        .half--screen-l,
        .fifty--screen-l {{
            {{
                width: 50%
            }}
        }}
        .forty--screen-l {{
            {{
                width: 40%
            }}
        }}
        .thirty--screen-l {{
            {{
                width: 30%
            }}
        }}
        .quarter--screen-l {{
            {{
                width: 25%
            }}
        }}
        .twenty--screen-l {{
            {{
                width: 20%
            }}
        }}
        .ten--screen-l {{
            {{
                width: 10%
            }}
        }}
    }}
}}

@media only screen and (min-width: 1440px) {{
    {{
        .hide--screen-xl {{
            {{
                display: none
            }}
        }}
        .show--screen-xl,
        .r-block--screen-xl {{
            {{
                display: block
            }}
        }}
        .block-mid--screen-xl {{
            {{
                margin-left: auto;
                margin-right: auto;
                display: block;
                float: none
            }}
        }}
        .pull-left--screen-xl,
        .fl--screen-xl {{
            {{
                float: left
            }}
        }}
        .pull-right--screen-xl,
        .fr--screen-xl {{
            {{
                float: right
            }}
        }}
        .pull-none--screen-xl,
        .killfloat--screen-xl {{
            {{
                float: none
            }}
        }}
        .whole--screen-xl {{
            {{
                width: 100%
            }}
        }}
        .ninety--screen-xl {{
            {{
                width: 90%
            }}
        }}
        .eighty--screen-xl {{
            {{
                width: 80%
            }}
        }}
        .seventy--screen-xl {{
            {{
                width: 70%
            }}
        }}
        .sixty--screen-xl {{
            {{
                width: 60%
            }}
        }}
        .half--screen-xl,
        .fifty--screen-xl {{
            {{
                width: 50%
            }}
        }}
        .forty--screen-xl {{
            {{
                width: 40%
            }}
        }}
        .thirty--screen-xl {{
            {{
                width: 30%
            }}
        }}
        .quarter--screen-xl {{
            {{
                width: 25%
            }}
        }}
        .twenty--screen-xl {{
            {{
                width: 20%
            }}
        }}
        .ten--screen-xl {{
            {{
                width: 10%
            }}
        }}
    }}
}}

@media only screen and (max-width: 864px) {{
    {{
        .port-half,
        .half--screen-m {{
            {{
                width: 50%;
                float: left;
                clear: none
            }}
        }}
        .block-mid--screen-m {{
            {{
                margin-left: auto;
                margin-right: auto;
                display: block;
                float: none
            }}
        }}
        .port-hide,
        .hide--screen-m {{
            {{
                display: none
            }}
        }}
        .port-block,
        .show--screen-m,
        .r-block--screen-m {{
            {{
                display: block
            }}
        }}
        .port-clear,
        .clear--screen-m,
        .r-clear--screen-m {{
            {{
                clear: both
            }}
        }}
        .port-killfloat,
        .port-pull-none,
        .port-float--none,
        .pull-none--screen-m,
        .killfloat--screen-m {{
            {{
                float: none
            }}
        }}
        .whole--screen-m {{
            {{
                width: 100%
            }}
        }}
        .ninety--screen-m {{
            {{
                width: 90%
            }}
        }}
        .eighty--screen-m {{
            {{
                width: 80%
            }}
        }}
        .seventy--screen-m {{
            {{
                width: 70%
            }}
        }}
        .sixty--screen-m {{
            {{
                width: 60%
            }}
        }}
        .half--screen-m,
        .fifty--screen-m {{
            {{
                width: 50%
            }}
        }}
        .forty--screen-m {{
            {{
                width: 40%
            }}
        }}
        .thirty--screen-m {{
            {{
                width: 30%
            }}
        }}
        .quarter--screen-m {{
            {{
                width: 25%
            }}
        }}
        .twenty--screen-m {{
            {{
                width: 20%
            }}
        }}
        .ten--screen-m {{
            {{
                width: 10%
            }}
        }}
    }}
}}

@media only screen and (max-width: 590px) {{
    {{
        html {{
            {{
                min-width: initial
            }}
        }}
        .whole,
        .half,
        .third,
        .twothird,
        .twothirds,
        .quarter,
        .threequarter,
        .threequarters,
        .sixth,
        .eighth,
        .ninety,
        .eighty,
        .seventy,
        .sixty,
        .fifty,
        .forty,
        .thirty,
        .twenty,
        .ten {{
            {{
                width: 100%;
                margin: 0 auto
            }}
        }}
        img.half,
        img.third,
        img.twothird,
        img.quarter,
        img.threequarter,
        img.sixth {{
            {{
                width: auto
            }}
        }}
        .palm-half,
        .half--screen-s {{
            {{
                width: 50%;
                float: left;
                clear: none
            }}
        }}
        .block-mid--screen-s {{
            {{
                margin-left: auto;
                margin-right: auto;
                display: block;
                float: none
            }}
        }}
        .palm-hide,
        .hide--screen-s {{
            {{
                display: none
            }}
        }}
        .palm-block,
        .show--screen-s,
        .r-block--screen-s {{
            {{
                display: block
            }}
        }}
        .palm-clear,
        .clear--screen-s,
        .r-clear--screen-s {{
            {{
                clear: both
            }}
        }}
        .palm-killfloat,
        .palm-pull-none,
        .palm-float--none,
        .killfloat-s,
        .pull-none--screen-s {{
            {{
                float: none
            }}
        }}
        .whole--screen-s {{
            {{
                width: 100%
            }}
        }}
        .ninety--screen-s {{
            {{
                width: 90%
            }}
        }}
        .eighty--screen-s {{
            {{
                width: 80%
            }}
        }}
        .seventy--screen-s {{
            {{
                width: 70%
            }}
        }}
        .sixty--screen-s {{
            {{
                width: 60%
            }}
        }}
        .half--screen-s,
        .fifty--screen-s {{
            {{
                width: 50%
            }}
        }}
        .forty--screen-s {{
            {{
                width: 40%
            }}
        }}
        .thirty--screen-s {{
            {{
                width: 30%
            }}
        }}
        .quarter--screen-s {{
            {{
                width: 25%
            }}
        }}
        .twenty--screen-s {{
            {{
                width: 20%
            }}
        }}
        .ten--screen-s {{
            {{
                width: 10%
            }}
        }}
    }}
}}

@media only screen and (max-width: 425px) {{
    {{
        .half--screen-xs {{
            {{
                width: 50%;
                float: left;
                clear: none
            }}
        }}
        .hide--screen-xs {{
            {{
                display: none
            }}
        }}
        .show--screen-xs,
        .r-block--screen-xs {{
            {{
                display: block
            }}
        }}
        .clear--screen-xs,
        .r-clear--screen-xs {{
            {{
                clear: both
            }}
        }}
        .killfloat-xs,
        .pull-none--screen-xs {{
            {{
                float: none
            }}
        }}
        .whole--screen-xs {{
            {{
                width: 100%
            }}
        }}
        .ninety--screen-xs {{
            {{
                width: 90%
            }}
        }}
        .eighty--screen-xs {{
            {{
                width: 80%
            }}
        }}
        .seventy--screen-xs {{
            {{
                width: 70%
            }}
        }}
        .sixty--screen-xs {{
            {{
                width: 60%
            }}
        }}
        .half--screen-xs,
        .fifty--screen-xs {{
            {{
                width: 50%
            }}
        }}
        .forty--screen-xs {{
            {{
                width: 40%
            }}
        }}
        .thirty--creen-xs {{
            {{
                width: 30%
            }}
        }}
        .quarter--screen-xs {{
            {{
                width: 25%
            }}
        }}
        .twenty--screen-xs {{
            {{
                width: 20%
            }}
        }}
        .ten--screen-xs {{
            {{
                width: 10%
            }}
        }}
    }}
}}

.tile--cat .tile__content a,
.tile--cat .tile__content br {{
    {{
        display: none
    }}
}}

.zci-wrap {{
    {{
        padding: 0 !important
    }}
}}

#error_homepage {{
    {{
        text-align: center;
        font-weight: bold;
        display: none
    }}
}}

#bottom_spacing2 {{
    {{
        padding-bottom: 225px
    }}
}}

#twitter_status {{
    {{
        position: fixed;
        left: 0;
        bottom: 0;
        width: 48px;
        height: 48px;
        z-index: 20
    }}
}}

.leaflet-tile {{
    {{
        -webkit-backface-visibility: inherit !important
    }}
}}
 </style>
 <body>
 {body}
 </body>
"""

ROW = """
<div class="zci  zci--about is-active" id="zci-about">
  <div class="c-info--cw  cw"><div class="zci__main  c-info">
    <div class="zci__body"><div class="c-info__body">
      <h1 class="c-info__title has-sub" style="padding-bottom: 10px;">
          <a href="{source_url}" title="{title} " style="text-decoration: none;">{title}</a><span class="c-info__title__sub ">JavaScript</span>
        </h1>
        <div class="c-info__content chomp js-ellipsis">
          {abstract}
        </div>
          <div class="c-info__links">
            <a class="c-info__link  c-info__link--chomp  chomp--link  js-chomp-link  sep--after">
              <i class="chomp--link__icn"></i>
              <span class="chomp--link__mr">Show More</span>
              <span class="chomp--link__ls">Show Less</span>
            </a>
            <a href="{source_url}" class="c-info__link"><img width="16" height="16" class="zci__more-at__icon" src="https://icons.duckduckgo.com/ip2/developer.mozilla.org.ico">More at Mozilla Developer Network</a>
          </div>
        </div>
      </div>
    </div>
  </div>
</div>
"""

def run(infname, outfname):
    infile = open(infname)
    reader = csv.DictReader(infile, FatWriter.FIELDS, dialect='excel-tab')
    with open(outfname, 'w') as outfile:
        rows = []
        for line in reader:
            rows.append(ROW.format(**line))
        body = '\n'.join(rows)
        outfile.write(HTML.format(body=body).replace('\\n', '\n'))

if __name__ == '__main__':
    infname = 'output.txt'
    outfname = 'preview.html'
    run(infname, outfname)
