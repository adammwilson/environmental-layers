<map version="1.0.0">
<!-- To view this file, download free mind mapping software FreeMind from http://freemind.sourceforge.net -->
<node CREATED="1337693448049" ID="ID_499007520" MODIFIED="1337805747728">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      <font color="#ff0006" size="5"><b>Climate processing</b></font>
    </p>
  </body>
</html></richcontent>
<node CREATED="1337693478939" FOLDED="true" ID="ID_976653303" MODIFIED="1337805747721" POSITION="left" TEXT="Download/preprocess data">
<icon BUILTIN="full-1"/>
<node CREATED="1337693492611" FOLDED="true" ID="ID_1622023543" MODIFIED="1337805747714">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      LST (MOD11A1) &#160;<font color="#149900">(JR&amp;BP)</font>
    </p>
  </body>
</html></richcontent>
<node CREATED="1337693771408" FOLDED="true" ID="ID_1881714255" LINK="https://projects.nceas.ucsb.edu/nceas/projects/environment-and-orga/repository/revisions/01b3830e935fd53889c078715af701506dc1bc97/entry/climate/extra/aggregate-daily-lst.py" MODIFIED="1337805747713">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      aggregate-daily-lst.py
    </p>
  </body>
</html></richcontent>
<node CREATED="1337696030080" ID="ID_345604069" MODIFIED="1337696035945" TEXT="Download tiles"/>
<node CREATED="1337696018352" FOLDED="true" ID="ID_883428808" MODIFIED="1337805747712" TEXT="QC flag handling, keep the following:">
<node CREATED="1337696078989" ID="ID_1653269019" MODIFIED="1337696132242" TEXT="First two QA bits &#x201c;00&#x201d;"/>
<node CREATED="1337696145379" ID="ID_1126403858" MODIFIED="1337696153065" TEXT="First two &#x201c;01&#x201d; and second two &#x201c;00&#x201d;"/>
<node CREATED="1337696166656" ID="ID_178027117" MODIFIED="1337696188024" TEXT="Explore &quot;LST error flags&quot; and &quot;clear-sky coverage&quot;"/>
</node>
<node CREATED="1337696056429" FOLDED="true" ID="ID_1676442262" MODIFIED="1337805747712" TEXT="Calculate monthly climatologies">
<node CREATED="1337696213956" ID="ID_284684278" MODIFIED="1337696221719" TEXT="LST day for tmax"/>
<node CREATED="1337696223377" ID="ID_1571291484" MODIFIED="1337696227030" TEXT="LST night for tmin"/>
</node>
</node>
</node>
<node CREATED="1337693745220" FOLDED="true" ID="ID_884884556" MODIFIED="1337805747716">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Cloud Product (MOD06_L2) <font color="#019c01">(AW)</font>
    </p>
  </body>
</html></richcontent>
<node CREATED="1337694612149" FOLDED="true" ID="ID_1573194506" MODIFIED="1337805747715" TEXT="aggregate-swath-mod06.r">
<node CREATED="1337695522534" ID="ID_560068855" MODIFIED="1337695635196" TEXT="Download swath data (not easy due to untiled format)"/>
<node CREATED="1337695636466" ID="ID_567730294" MODIFIED="1337695756542" TEXT="swath-&gt;grid"/>
<node CREATED="1337695762202" ID="ID_1779987171" MODIFIED="1337695768853" TEXT="QC flag handling"/>
<node CREATED="1337695773345" ID="ID_454418278" MODIFIED="1337695798667" TEXT="monthly summaries"/>
</node>
</node>
<node CREATED="1337693858549" FOLDED="true" ID="ID_132329517" MODIFIED="1337805747717" TEXT="Landcover">
<node CREATED="1337694673911" FOLDED="true" ID="ID_340925422" MODIFIED="1337805747717" TEXT="Jetz consensus product">
<icon BUILTIN="flag-green"/>
<node CREATED="1337713232568" ID="ID_1681449624" MODIFIED="1337713428348">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Landcover 1 &amp; 3 (grass &amp; forest)<font color="#028f0c">&#160;(BP)</font>
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1337713279548" ID="ID_1698377334" MODIFIED="1337713291975" TEXT="Add Barren, Urban, Agriculture"/>
<node CREATED="1337713299696" ID="ID_1135739112" MODIFIED="1337713346632" TEXT="Add spatial aggregation around points (2km, 4km?)"/>
</node>
</node>
<node CREATED="1337713444927" FOLDED="true" ID="ID_1699470382" MODIFIED="1337805747721" TEXT="Met Station Data">
<node CREATED="1337693880467" FOLDED="true" ID="ID_303589508" MODIFIED="1337805747719" TEXT="Station Data (GHCN)">
<node CREATED="1337694760649" FOLDED="true" ID="ID_980361932" LINK="https://projects.nceas.ucsb.edu/nceas/projects/environment-and-orga/repository/revisions/c7235eaebbe073c3d32c29e1fbd340bba637c7df/entry/climate/extra/ghcn-to-psql.R" MODIFIED="1337805747718">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      ghcn-to-psql.R <font color="#06a401">(JR)</font>
    </p>
  </body>
</html></richcontent>
<node CREATED="1337695211849" ID="ID_1420445695" MODIFIED="1337695328753" TEXT="Download .dly files"/>
<node CREATED="1337695229751" ID="ID_1081105282" MODIFIED="1337695388785" TEXT="Convert to tabular format">
<icon BUILTIN="flag-green"/>
</node>
<node CREATED="1337695241037" ID="ID_221714299" MODIFIED="1337695390305" TEXT="insert into database">
<icon BUILTIN="flag-green"/>
</node>
</node>
<node CREATED="1337695866690" ID="ID_82660518" MODIFIED="1337695981028" TEXT="Monthly GHCN: construct from daily or use Monthly GHCN product"/>
</node>
<node CREATED="1337693971567" FOLDED="true" ID="ID_1112385171" MODIFIED="1337805747720" TEXT="Station Data (other)">
<node CREATED="1337694798883" ID="ID_1203773837" MODIFIED="1337695860669" TEXT="If we pursue anomaly approach, we may want to use WMO climatologies"/>
<node CREATED="1337722524115" FOLDED="true" ID="ID_277739818" LINK="https://docs.google.com/spreadsheet/ccc?key=0Amdov1dAmji5dEtMRUM1MUZmMUZ4YlRWQzZzLV9LNlE" MODIFIED="1337805747720" TEXT="Literature review of (available?) data">
<node CREATED="1337722559077" ID="ID_909532490" MODIFIED="1337722564374" TEXT="WorldClim data"/>
<node CREATED="1337696712398" ID="ID_1489094293" MODIFIED="1337699833653">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      South African <font color="#028716">(AW)</font>
    </p>
  </body>
</html></richcontent>
<icon BUILTIN="male1"/>
</node>
<node CREATED="1337696721882" ID="ID_371829888" MODIFIED="1337696856847" TEXT="Neotropics?"/>
<node CREATED="1337696736013" ID="ID_941927867" MODIFIED="1337699849677">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Japan? <font color="#028b08">(AW)</font>
    </p>
  </body>
</html></richcontent>
<icon BUILTIN="male1"/>
</node>
</node>
</node>
<node CREATED="1337694881142" FOLDED="true" ID="ID_424054784" MODIFIED="1337805747720">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Quality Control <font color="#068402">(AW &amp; BP)</font>
    </p>
  </body>
</html></richcontent>
<node CREATED="1337695172158" ID="ID_817228390" MODIFIED="1337695177796" TEXT="Update database with QC flags"/>
<node CREATED="1337694928723" ID="ID_1036738506" MODIFIED="1337695190937" TEXT="outliers, runs, etc"/>
<node CREATED="1337695400785" ID="ID_679688223" MODIFIED="1337695412462" TEXT="Adam has example script"/>
<node CREATED="1337723193346" ID="ID_532427724" MODIFIED="1337723215371" TEXT="Station Selection (WorldClim style priorities)"/>
</node>
<node CREATED="1337699923359" ID="ID_1034401963" MODIFIED="1337699950943" TEXT="Spatial Query (submit polygon and date to get table of observations)"/>
</node>
<node CREATED="1337713572766" FOLDED="true" ID="ID_1211719545" MODIFIED="1337805747721" TEXT="Other data">
<node CREATED="1337713577167" ID="ID_1584845754" MODIFIED="1337713582360" TEXT="NCEP Reanalysis?"/>
</node>
</node>
<node CREATED="1337696640713" FOLDED="true" ID="ID_1410709754" MODIFIED="1337805747721" POSITION="left" TEXT="Regions">
<icon BUILTIN="full-2"/>
<node CREATED="1337697035100" ID="ID_439992455" MODIFIED="1337697036524" TEXT="Oregon"/>
<node CREATED="1337697039107" ID="ID_538311410" MODIFIED="1337697041013" TEXT="South Africa"/>
<node CREATED="1337697050599" ID="ID_1880530792" MODIFIED="1337697053267" TEXT="East Africa"/>
</node>
<node CREATED="1337694217407" FOLDED="true" ID="ID_1692790642" MODIFIED="1337805747722" POSITION="left" TEXT="Subset">
<icon BUILTIN="full-3"/>
<node CREATED="1337694227599" FOLDED="true" ID="ID_907402330" MODIFIED="1337805747721" TEXT="Spatial">
<node CREATED="1337696593506" FOLDED="true" ID="ID_219701496" MODIFIED="1337805747721" TEXT="Spatial Tiling">
<node CREATED="1337696931165" ID="ID_255962886" MODIFIED="1337696971387" TEXT="Tiling depends on methods (will we need data from surrounding tiles or just stations)?"/>
<node CREATED="1337694275313" ID="ID_854736297" MODIFIED="1337696994157" TEXT="Use stations from outside region to reduce edge effects"/>
</node>
</node>
<node CREATED="1337694234192" FOLDED="true" ID="ID_1680696121" MODIFIED="1337805747722" TEXT="Temporal">
<node CREATED="1337697075948" ID="ID_1400143470" MODIFIED="1337697081425" TEXT="Subset to 1970-2010"/>
<node CREATED="1337697084530" ID="ID_907061196" MODIFIED="1337697092004" TEXT="Day-by-day interpolations"/>
</node>
<node CREATED="1337694241728" FOLDED="true" ID="ID_644499787" MODIFIED="1337805747722" TEXT="Reproject">
<node CREATED="1337694248230" ID="ID_867337175" MODIFIED="1337694263148" TEXT="Use local projection (UTM?) for interpolation?"/>
<node CREATED="1337721280170" ID="ID_1156119627" MODIFIED="1337721292456" TEXT="Fit in UTM and predict into translated cell centroids?"/>
</node>
<node CREATED="1337701235770" FOLDED="true" ID="ID_1775941968" MODIFIED="1337805747722" TEXT="Prepare Design matrix for modelling">
<node CREATED="1337701279162" ID="ID_1661261436" MODIFIED="1337764353373" TEXT="One day"/>
<node CREATED="1337701328594" ID="ID_274392765" MODIFIED="1337701337057" TEXT="One variable (tmax, tmin, or ppt)"/>
<node CREATED="1337701338233" ID="ID_241629300" MODIFIED="1337701345167" TEXT="One tile"/>
</node>
</node>
<node CREATED="1337697213185" FOLDED="true" ID="ID_15031549" MODIFIED="1337805747727" POSITION="right" TEXT="Interpolation">
<icon BUILTIN="full-4"/>
<node CREATED="1337697235796" FOLDED="true" ID="ID_1692618034" MODIFIED="1337805747722" TEXT="Temporal resolution (1970-2010)">
<node CREATED="1337697244020" ID="ID_1640475194" MODIFIED="1337697245792" TEXT="Daily"/>
<node CREATED="1337697246995" ID="ID_1369257827" MODIFIED="1337697249860" TEXT="Monthly"/>
<node CREATED="1337697251140" ID="ID_577926600" MODIFIED="1337697256708" TEXT="Monthly Climatologies"/>
</node>
<node CREATED="1337697226173" FOLDED="true" ID="ID_1296935309" MODIFIED="1337805747725" TEXT="Methods">
<node CREATED="1337701916996" FOLDED="true" ID="ID_880392640" MODIFIED="1337805747722" TEXT="Response variables">
<node CREATED="1337701929541" ID="ID_1427601938" MODIFIED="1337701953760">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Temperature (tmax &amp; tmin)<font color="#017708">&#160;(BP)</font>
    </p>
  </body>
</html></richcontent>
</node>
<node CREATED="1337701957896" ID="ID_1619372413" MODIFIED="1337702014751">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Precipitation <font color="#01830b">(AW)</font>
    </p>
  </body>
</html></richcontent>
</node>
</node>
<node CREATED="1337697677834" FOLDED="true" ID="ID_599360131" MODIFIED="1337805747723" TEXT="Input variables">
<node CREATED="1337697688482" ID="ID_1871931734" MODIFIED="1337697695668" TEXT="Latitude"/>
<node CREATED="1337697691388" ID="ID_1153921404" MODIFIED="1337697698251" TEXT="Longitude"/>
<node CREATED="1337697699190" ID="ID_1767873058" MODIFIED="1337697703651" TEXT="Distance to Ocean"/>
<node CREATED="1337697704782" ID="ID_1688387348" MODIFIED="1337697714221" TEXT="Aspect (eastness &amp; northness)"/>
<node CREATED="1337697715634" ID="ID_995593396" MODIFIED="1337697724360" TEXT="Elevation"/>
<node CREATED="1337697725310" ID="ID_766953470" MODIFIED="1337697785175" TEXT="Land Surface Temperature (LST)"/>
<node CREATED="1337697795196" ID="ID_148550770" MODIFIED="1337697823186" TEXT="Cloud product?"/>
<node CREATED="1337697899207" FOLDED="true" ID="ID_1471302870" MODIFIED="1337805747723" TEXT="Land cover">
<node CREATED="1337697904435" ID="ID_128992060" MODIFIED="1337697910821" TEXT="interactions with LST?"/>
</node>
<node CREATED="1337714249015" FOLDED="true" ID="ID_1134140871" MODIFIED="1337805747723" TEXT="Others?">
<node CREATED="1337714255701" ID="ID_403437508" MODIFIED="1337714272535" TEXT="other terrain variables?"/>
<node CREATED="1337714285640" ID="ID_1934533760" MODIFIED="1337714302998" TEXT="coarse gridded weather (NCEP reanalysis)"/>
</node>
</node>
<node CREATED="1337697653796" FOLDED="true" ID="ID_1967178138" MODIFIED="1337805747724" TEXT="&apos;Raw&apos; values">
<node CREATED="1337697269453" FOLDED="true" ID="ID_1006915660" MODIFIED="1337805747723" TEXT="GAM">
<node CREATED="1337701358195" FOLDED="true" ID="ID_1552803013" MODIFIED="1337805747723" TEXT="Input variables">
<node CREATED="1337701442885" ID="ID_1963630966" MODIFIED="1337701451623" TEXT="Daily vs. monthly mean LST"/>
</node>
<node CREATED="1337701412448" FOLDED="true" ID="ID_1453574000" MODIFIED="1337805747723" TEXT="Interactions">
<node CREATED="1337701456001" ID="ID_1273197967" MODIFIED="1337701465854" TEXT="LST &amp; Landcover"/>
<node CREATED="1337701469632" ID="ID_651464033" MODIFIED="1337701472569" TEXT="aspect"/>
<node CREATED="1337701494058" ID="ID_1659159243" MODIFIED="1337701499886" TEXT="Latitude/Longitude"/>
</node>
<node CREATED="1337714980501" ID="ID_574328154" MODIFIED="1337714992688" TEXT="Spatial structure in error term?"/>
</node>
<node CREATED="1337697278338" FOLDED="true" ID="ID_1725153104" MODIFIED="1337805747724" TEXT="Co-kriging">
<node CREATED="1337701520770" FOLDED="true" ID="ID_1408791547" MODIFIED="1337805747724" TEXT="Variogram">
<node CREATED="1337701525586" ID="ID_1732269465" MODIFIED="1337701534160" TEXT="Fit with LST data"/>
<node CREATED="1337701535206" ID="ID_1503144865" MODIFIED="1337701541936" TEXT="Fit with station observations"/>
<node CREATED="1337701544595" ID="ID_1962531134" MODIFIED="1337701564932" TEXT="explore various variograms (automap)"/>
<node CREATED="1337701569795" ID="ID_400352692" MODIFIED="1337701574613" TEXT="Bayesian?"/>
</node>
</node>
<node CREATED="1337697281075" FOLDED="true" ID="ID_1837699933" MODIFIED="1337805747724" TEXT="GWR">
<node CREATED="1337701637242" ID="ID_912490046" MODIFIED="1337701682376" TEXT="same as above"/>
<node CREATED="1337701694620" ID="ID_71289629" MODIFIED="1337701711362" TEXT="range setting set (spgwr package)"/>
<node CREATED="1337701715058" FOLDED="true" ID="ID_424191977" MODIFIED="1337805747724" TEXT="Validation">
<node CREATED="1337701719851" ID="ID_1543877575" MODIFIED="1337701736306" TEXT="Range based on training dataset"/>
<node CREATED="1337701738472" ID="ID_1835145782" MODIFIED="1337701775835" TEXT="Krig slopes based on training data and use for predictions"/>
</node>
</node>
</node>
<node CREATED="1337697646348" FOLDED="true" ID="ID_1127846820" MODIFIED="1337805747724" TEXT="Anomaly Approach">
<node CREATED="1337697269453" ID="ID_1974814189" MODIFIED="1337697277159" TEXT="GAM"/>
<node CREATED="1337697278338" ID="ID_1114918308" MODIFIED="1337722375384" TEXT="cokriging"/>
<node CREATED="1337697281075" ID="ID_1403425851" MODIFIED="1337697282244" TEXT="GWR"/>
</node>
<node CREATED="1337697825505" ID="ID_1235804707" MODIFIED="1337697854510" TEXT="Backup method (for times/locations that primary method fails)"/>
</node>
<node CREATED="1337700453400" FOLDED="true" ID="ID_1764057631" MODIFIED="1337805747727" TEXT="Assessment">
<node CREATED="1337697587352" FOLDED="true" ID="ID_1111409905" MODIFIED="1337805747725">
<richcontent TYPE="NODE"><html>
  <head>
    
  </head>
  <body>
    <p>
      Literature Review: Compare to other interpolation studies <font color="#01a00a">(BP &amp; AW)</font>
    </p>
  </body>
</html></richcontent>
<node CREATED="1337702592191" ID="ID_1109082387" MODIFIED="1337702597850" TEXT="Mendely shared library"/>
<node CREATED="1337722297901" ID="ID_702952208" MODIFIED="1337722301750" TEXT="Table of methods"/>
<node CREATED="1337722303924" ID="ID_437946867" MODIFIED="1337722313586" TEXT="Explanation of similarities and differences"/>
<node CREATED="1337722320078" ID="ID_1695809130" MODIFIED="1337722338669" TEXT="GAM vs Spline vs ANUSPLIN"/>
</node>
<node CREATED="1337697289184" FOLDED="true" ID="ID_800767423" MODIFIED="1337805747726" TEXT="Validation (holdout stations)">
<node CREATED="1337721537535" FOLDED="true" ID="ID_1440764691" MODIFIED="1337805747725" TEXT="metrics">
<node CREATED="1337697291945" ID="ID_1279187065" MODIFIED="1337697296054" TEXT="RMSE"/>
<node CREATED="1337697297260" ID="ID_676994378" MODIFIED="1337697298703" TEXT="MAE"/>
<node CREATED="1337697300612" ID="ID_1888790124" MODIFIED="1337697344604" TEXT="Bias (mean error)"/>
<node CREATED="1337697381000" ID="ID_835799389" MODIFIED="1337697385278" TEXT="R^2"/>
<node CREATED="1337700496780" FOLDED="true" ID="ID_213135038" MODIFIED="1337805747725" TEXT="Precipitation">
<node CREATED="1337700502834" ID="ID_316342827" MODIFIED="1337700541140" TEXT="sensitivity/specificity"/>
<node CREATED="1337700543098" ID="ID_1199492789" MODIFIED="1337700643961" TEXT="Relative error rather than absolute"/>
</node>
</node>
<node CREATED="1337721548098" FOLDED="true" ID="ID_1213593936" MODIFIED="1337805747725" TEXT="Comparisons">
<node CREATED="1337721587292" ID="ID_1004288967" MODIFIED="1337721598950" TEXT="spatiotemporal distribution of metrics"/>
<node CREATED="1337721600641" ID="ID_1872295961" MODIFIED="1337721626887" TEXT="daily vs monthly vs annual, etc."/>
<node CREATED="1337721689405" ID="ID_545588867" MODIFIED="1337721700242" TEXT="median, range, quantiles"/>
</node>
</node>
<node CREATED="1337716683210" FOLDED="true" ID="ID_211611939" MODIFIED="1337805747726" TEXT="Model specific parameters">
<node CREATED="1337716706972" FOLDED="true" ID="ID_1651119243" MODIFIED="1337805747726" TEXT="GAM">
<node CREATED="1337716712653" ID="ID_1755673609" MODIFIED="1337716728239" TEXT="variance explained per variable"/>
<node CREATED="1337716735408" ID="ID_1361223923" MODIFIED="1337716737566" TEXT="AIC"/>
<node CREATED="1337716739148" ID="ID_695482636" MODIFIED="1337716767750" TEXT="GCV (smoothing term)"/>
<node CREATED="1337716825980" ID="ID_1977219103" MODIFIED="1337716832358" TEXT="RMSE (fitting dataset)"/>
<node CREATED="1337716846514" ID="ID_520856278" MODIFIED="1337716894690" TEXT="prediction uncertainties"/>
</node>
<node CREATED="1337716804398" FOLDED="true" ID="ID_811989997" MODIFIED="1337805747726" TEXT="Kriging">
<node CREATED="1337716809326" ID="ID_931340421" MODIFIED="1337716844789" TEXT="variogram parameters"/>
<node CREATED="1337716846514" ID="ID_690500038" MODIFIED="1337716894690" TEXT="prediction uncertainties"/>
</node>
<node CREATED="1337716908303" FOLDED="true" ID="ID_1845621632" MODIFIED="1337805747726" TEXT="GWR">
<node CREATED="1337716911372" ID="ID_849699598" MODIFIED="1337716944161" TEXT="Summary of localized slopes"/>
</node>
</node>
<node CREATED="1337697463443" FOLDED="true" ID="ID_1768628491" MODIFIED="1337805747726" TEXT="Compare station monthly means with other products">
<node CREATED="1337697469649" ID="ID_1131012394" MODIFIED="1337697472164" TEXT="WorldClim"/>
<node CREATED="1337697473253" ID="ID_188472453" MODIFIED="1337697474817" TEXT="PRISM"/>
</node>
</node>
<node CREATED="1337698551579" ID="ID_450839519" MODIFIED="1337698562830" TEXT="Include pixel-by-pixel uncertainties?"/>
<node CREATED="1337700083465" ID="ID_68164595" MODIFIED="1337700093376" TEXT="parallelizing procedures"/>
</node>
<node CREATED="1337764089640" ID="ID_1044144023" MODIFIED="1337764098072" POSITION="right" TEXT="Derived Climate">
<icon BUILTIN="full-5"/>
</node>
<node CREATED="1337698420065" FOLDED="true" ID="ID_715327186" MODIFIED="1337805747728" POSITION="right" TEXT="Data Distribution">
<icon BUILTIN="full-6"/>
<node CREATED="1337698443012" FOLDED="true" ID="ID_1461975798" MODIFIED="1337805747727" TEXT="Projection">
<node CREATED="1337698449778" ID="ID_1881775731" LINK="http://en.wikipedia.org/wiki/Behrmann_projection" MODIFIED="1337698477324" TEXT="Berhmann"/>
<node CREATED="1337805719991" ID="ID_325422666" MODIFIED="1337805723913" TEXT="WGS84"/>
</node>
<node CREATED="1337805674196" FOLDED="true" ID="ID_60771643" MODIFIED="1337805747727" TEXT="Server Possibilities">
<node CREATED="1337698481093" FOLDED="true" ID="ID_1915639097" LINK="http://iridl.ldeo.columbia.edu/" MODIFIED="1337805747727" TEXT="Use IRI&apos;s Data Library?">
<node CREATED="1337698517223" ID="ID_23787097" MODIFIED="1337698541435" TEXT="Automatic browsing, subsetting, transformation to various file formats"/>
</node>
<node CREATED="1337805691276" ID="ID_1361948703" MODIFIED="1337805696361" TEXT="Map of life infrastructure?"/>
</node>
<node CREATED="1337805703484" FOLDED="true" ID="ID_1426105560" MODIFIED="1337805747727" TEXT="Formats">
<node CREATED="1337805707748" ID="ID_1957761582" MODIFIED="1337805710559" TEXT="netCDF"/>
<node CREATED="1337805711980" ID="ID_860118197" MODIFIED="1337805714590" TEXT="geotiff"/>
</node>
</node>
</node>
</map>
