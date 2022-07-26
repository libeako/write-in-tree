<map version="freeplane 1.9.13">
<!--To view this file, download free mind mapping software Freeplane from https://www.freeplane.org -->
<node TEXT="*-&gt; Demonstration and Tutorial of WriteInTree" ID="ID_1791247219"><hook NAME="MapStyle">
    <properties edgeColorConfiguration="#808080ff,#ff0000ff,#0000ffff,#00ff00ff,#ff00ffff,#00ffffff,#7c0000ff,#00007cff,#007c00ff,#7c007cff,#007c7cff,#7c7c00ff" fit_to_viewport="false"/>

<map_styles>
<stylenode LOCALIZED_TEXT="styles.root_node" STYLE="oval" UNIFORM_SHAPE="true" VGAP_QUANTITY="24 pt">
<font SIZE="24"/>
<stylenode LOCALIZED_TEXT="styles.predefined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="default" ID="ID_271890427" ICON_SIZE="12 pt" COLOR="#000000" STYLE="fork">
<arrowlink SHAPE="CUBIC_CURVE" COLOR="#000000" WIDTH="2" TRANSPARENCY="200" DASH="" FONT_SIZE="9" FONT_FAMILY="SansSerif" DESTINATION="ID_271890427" STARTARROW="NONE" ENDARROW="DEFAULT"/>
<font NAME="SansSerif" SIZE="10" BOLD="false" ITALIC="false"/>
<richcontent CONTENT-TYPE="plain/auto" TYPE="DETAILS"/>
<richcontent TYPE="NOTE" CONTENT-TYPE="plain/auto"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.details"/>
<stylenode LOCALIZED_TEXT="defaultstyle.attributes">
<font SIZE="9"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.note" COLOR="#000000" BACKGROUND_COLOR="#ffffff" TEXT_ALIGN="LEFT"/>
<stylenode LOCALIZED_TEXT="defaultstyle.floating">
<edge STYLE="hide_edge"/>
<cloud COLOR="#f0f0f0" SHAPE="ROUND_RECT"/>
</stylenode>
<stylenode LOCALIZED_TEXT="defaultstyle.selection" BACKGROUND_COLOR="#4e85f8" BORDER_COLOR_LIKE_EDGE="false" BORDER_COLOR="#4e85f8"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.user-defined" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="styles.topic" COLOR="#18898b" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subtopic" COLOR="#cc3300" STYLE="fork">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.subsubtopic" COLOR="#669900">
<font NAME="Liberation Sans" SIZE="10" BOLD="true"/>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.important" ID="ID_67550811">
<icon BUILTIN="yes"/>
<arrowlink COLOR="#003399" TRANSPARENCY="255" DESTINATION="ID_67550811"/>
</stylenode>
</stylenode>
<stylenode LOCALIZED_TEXT="styles.AutomaticLayout" POSITION="right" STYLE="bubble">
<stylenode LOCALIZED_TEXT="AutomaticLayout.level.root" COLOR="#000000" STYLE="oval" SHAPE_HORIZONTAL_MARGIN="10 pt" SHAPE_VERTICAL_MARGIN="10 pt">
<font SIZE="18"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,1" COLOR="#0033ff">
<font SIZE="16"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,2" COLOR="#00b439">
<font SIZE="14"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,3" COLOR="#990000">
<font SIZE="12"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,4" COLOR="#111111">
<font SIZE="10"/>
</stylenode>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,5"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,6"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,7"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,8"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,9"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,10"/>
<stylenode LOCALIZED_TEXT="AutomaticLayout.level,11"/>
</stylenode>
</stylenode>
</map_styles>
</hook>
<node TEXT="This is a software that transforms text-tree into html website. The name &quot;write-in-tree&quot; is meant to mean both a technology and a concrete software [the &quot;compiler&quot;]." POSITION="right" ID="ID_446310165"/>
<node TEXT="Is for those who want to write a book [or article], want to write and maintain it fast, without hassle, but beside the comfort of author also want to give the readers a convenient html website format." POSITION="right" ID="ID_1534207874"/>
<node TEXT="This software [the whole thing] is free." POSITION="right" ID="ID_291836033"/>
<node TEXT="This documentation is about language version 2." POSITION="right" ID="ID_1526836883"/>
<node TEXT="*** The Bad" POSITION="right" ID="ID_105200887">
<node TEXT="Parallel work on a document through multiple threads is usually not possible." ID="ID_447801329">
<node TEXT="This is because the lack of good version controlling for tree structured data [like XML]." ID="ID_1891298457"/>
</node>
</node>
<node TEXT="*** The Good" POSITION="right" ID="ID_962281436">
<node TEXT="Very comfortable, fast, distraction-less writing and maintaining of the source document." ID="ID_809933437">
<node TEXT="Writing your book with this tech gives you perhaps the biggest possible comfort during writing - you do not have to deal with technical details like formatting at this stage, but only about the content and its structure. You can structure and restructure your content with big ease, so you can unleash your full authoring talent." ID="ID_499428721"/>
</node>
<node TEXT="Custom appearance style via CSS." ID="ID_1072565064"/>
<node TEXT="Links do not change by editing the source." ID="ID_129835508"/>
</node>
<node TEXT="*** Maturity" POSITION="right" ID="ID_1232349015">
<node TEXT="Though this tech is not mature yet, the author of it has already used it to write his book without any major problems and with great satisfaction." ID="ID_1761526856">
<node TEXT="link to that book" ID="ID_232129585">
<node TEXT="# links-to" ID="ID_374911017">
<node TEXT="external" ID="ID_1883863997"/>
<node TEXT="http://libeako.github.io" ID="ID_1872369794"/>
</node>
</node>
</node>
</node>
<node TEXT="*** Popularity" POSITION="right" ID="ID_164399876">
<node TEXT="Nobody uses or even knows about this tech known by the author of this tech." ID="ID_699435795"/>
</node>
<node TEXT="*** The Current Version " POSITION="right" ID="ID_1895828313">
<node TEXT="of the language described in this tutor is 1.1." ID="ID_1298108279"/>
</node>
<node TEXT="*-&gt; Basic workflow" POSITION="right" ID="ID_1701096323">
<node TEXT="You write your book&apos;s contents in text tree format first. A text tree is a tree of simple text lines." ID="ID_1538148038">
<node TEXT="*-&gt; Text Tree Editors" ID="ID_1375820022">
<node TEXT="Applications dedicated for simple text tree editing do not current exist [according to my knowledge], but 2 &quot;mindmap editors&quot; do exist." ID="ID_208360180"/>
<node TEXT="They are:" ID="ID_988442566">
<node TEXT="Freeplane" ID="ID_1898213544">
<node TEXT="# links-to" ID="ID_1217962855">
<node TEXT="external" ID="ID_1128677899"/>
<node TEXT="http://freeplane.sourceforge.net/wiki/index.php/Main_Page" ID="ID_189490486"/>
</node>
</node>
<node TEXT="Freemind" ID="ID_1464453876">
<node TEXT="# links-to" ID="ID_1099236299">
<node TEXT="external" ID="ID_1356938328"/>
<node TEXT="http://freemind.sourceforge.net/wiki/index.php/Main_Page" ID="ID_1374350475"/>
</node>
</node>
</node>
<node TEXT="WriteInTree currently builds and depends on them. You need to use one of them." ID="ID_684658794"/>
<node TEXT="They are both more than just text tree editors, they are mindmap editors. Still they can be used simply as text tree editors. They both are free software, they are mature, though they do contain bugs. They run on the Java platform." ID="ID_599611998"/>
<node TEXT="*-&gt; Installation" ID="ID_1609167169">
<node TEXT="On Windows the installation of these 2 software is very easy. Most Linux distributions probably have packaged them." ID="ID_463470084"/>
<node TEXT="Recommended Configuration" ID="ID_652748693">
<node TEXT="While they are promtly usable after installation, based on my own experience I suggest the following configuration right after installation." ID="ID_404913"/>
<node TEXT="In case of Freeplane:" ID="ID_656665140">
<node TEXT="In &quot;Environment&quot; -&gt; &quot;Save&quot;" ID="ID_379938942">
<node TEXT="&quot;Save folding&quot; := &quot;Never&quot;" ID="ID_84041838"/>
<node TEXT="&quot;Save used node-IDs only&quot; := false" ID="ID_530301020"/>
<node TEXT="&quot;Save modification times&quot; := false" ID="ID_8575050"/>
<node TEXT="These changes make version control easier." ID="ID_1930894383"/>
</node>
</node>
<node TEXT="In case of Freemind there are similar settings." ID="ID_1889031902"/>
</node>
</node>
<node TEXT="*-&gt; Usage" ID="ID_31782854">
<node TEXT="You Want to Use the Keyboard" ID="ID_1807785692">
<node TEXT="While you can use mouse intuitively in these applications, you really want to use keyboard, because it is nicely supported and provides super-fast editing ability." ID="ID_164721404">
<node TEXT="You can edit the text inside a node by F2." ID="ID_1240419648"/>
<node TEXT="You can fold, unfold nodes by the space key." ID="ID_1465165288"/>
<node TEXT="You can selected several nodes by holding down Shift." ID="ID_1753004324"/>
<node TEXT="You can move selected nodes by Ctrl + &lt;arrow keys&gt;." ID="ID_49487924"/>
<node TEXT="You can insert new empty nodes by Enter, Shift + enter, Insert, Shift + Insert." ID="ID_204477741"/>
</node>
</node>
<node TEXT="Rich formatting" ID="ID_725938257">
<node TEXT="As these applications are more than just outline editors, they allow rich formatting inside text nodes. But this compiler can not parse those correctly. So do not use this feature of your mindmap editor." ID="ID_716682419"/>
<node TEXT="The compiler will error and report such situations." ID="ID_399485340"/>
<node TEXT="In Freeplane you can easily convert a rich formatted node into a plain formatted one by pressing Alt+P." ID="ID_1104085797"/>
</node>
</node>
</node>
</node>
<node TEXT="When the structure and content of your book is ready then you have 2 tasks left:" ID="ID_379383956">
<node TEXT="Review your work in its entirity to improve its quality." ID="ID_732039018"/>
<node TEXT="Specify format how you want your book look like when rendered into html website. You do this by attaching special nodes to your mindmap. What special nodes and how they will control the translation into website is defined by the write-in-tree technology." ID="ID_1367020098"/>
</node>
<node TEXT="Often it is best to do these 2 taks at the same time." ID="ID_546928490"/>
</node>
<node TEXT="*-&gt; Getting WriteInTree" POSITION="right" ID="ID_890993384">
<node TEXT="You have to build the software from source yourself, from Haskell." ID="ID_1399987050"/>
<node TEXT=" You can download the source from here :" ID="ID_12088072">
<node TEXT="the core" ID="ID_696982228">
<node TEXT="WriteInTree" ID="ID_1741849269">
<node TEXT="# links-to" ID="ID_761381223">
<node TEXT="external" ID="ID_53936520"/>
<node TEXT="https://bitbucket.org/libeako/haskell-write-in-tree/src/default/" ID="ID_1215804581"/>
</node>
</node>
</node>
<node TEXT="the dependers" ID="ID_1145559369">
<node TEXT="Fana" ID="ID_212107269">
<node TEXT="# links-to" ID="ID_1089680994">
<node TEXT="external" ID="ID_1638445423"/>
<node TEXT="https://bitbucket.org/libeako/haskell-fana/src/default/" ID="ID_1598755028"/>
</node>
</node>
</node>
</node>
</node>
<node TEXT="*-&gt; Tutorial" POSITION="right" ID="ID_261606009">
<node TEXT="*-&gt; Hello World - your first written-in-tree book" ID="ID_1744670674">
<node TEXT="Create a folder for your future document." ID="ID_1611124129"/>
<node TEXT="Create a mindmap file in the document folder with the name &quot;tree.mm&quot;, and with some arbitrary content, just a simple node like &quot;Hello World&quot; suffices." ID="ID_485514684"/>
<node TEXT="Create the separate properties file in the document folder." ID="ID_461439345">
<node TEXT="It is a configuration of your project." ID="ID_1850335707"/>
<node TEXT="This file has the name &quot;properties.simco.text&quot;." ID="ID_1440443724"/>
<node TEXT="It uses the SimCo language." ID="ID_63893595">
<node TEXT="link to SimCo" ID="ID_430700241">
<node TEXT="# links-to" ID="ID_446916265">
<node TEXT="external" ID="ID_434133203"/>
<node TEXT="https://libeako.github.io/c/ID_1648412630.html" ID="ID_1754593090"/>
</node>
</node>
</node>
<node TEXT="The application can generate for you a default version of this file. I recommend you to use it." ID="ID_304394027"/>
</node>
<node TEXT="Compile." ID="ID_1710131487">
<node TEXT="Call this compiler with the command line: &quot;write-in-tree.exe --web -i &lt;path-to-the-document-folder&gt; -o &lt;path-of-the-output-folder&gt;&quot;" ID="ID_1464032321"/>
<node TEXT="Advice: put this command line into a script file for saving and possible later extensions. This script file in this tutorial will be referred to as the &quot;compile script&quot;." ID="ID_1284103143"/>
</node>
</node>
<node TEXT="*** Basics" ID="ID_1582677615">
<node TEXT="*** Syntax" ID="ID_1993443644">
<node TEXT="A tree node may be either a plain text node or a meta node." ID="ID_1176321787"/>
<node TEXT="Text nodes are usually the ones that will be visible be the reader and are the main content of your work. Meta nodes are instructions for the compiler how to format your content into html website. Meta nodes have the format &quot;# instruction&quot;." ID="ID_807391098"/>
<node TEXT="Text nodes are entered without any special syntax, just as they are. Meta nodes are prefixed by &quot;# &quot;. If you need to create a text node with prefix &quot;# &quot;, then do it with prefix &quot;## # &quot;, as prefix &quot;## &quot; instructs the software to treat the following as a plain text node." ID="ID_40501939"/>
</node>
<node TEXT="*** Semantics" ID="ID_1264139646">
<node TEXT="The text-tree represents an abstract website data format. Each subtree represents an element of the website. Elements are usually nested into other element." ID="ID_219154373"/>
<node TEXT="There are 2 basic categories of element types:" ID="ID_279967442">
<node TEXT="Inline - plain text, image." ID="ID_768997279"/>
<node TEXT="Structural - paragraph, section, list, page." ID="ID_1206981504"/>
</node>
</node>
</node>
<node TEXT="*** The Rich Text Tree Format" ID="ID_1408452053">
<node TEXT="This is a text tree format with some additional features, but still views the document as a tree of text lines." ID="ID_401763949"/>
<node TEXT="*** Labeling" ID="ID_963040019">
<node TEXT="You can create an &quot;id&quot; and a &quot;class&quot; meta node for a node under it. Unders them list the label values as simple text child-nodes." ID="ID_933333920"/>
</node>
</node>
<node TEXT="*** Css" ID="ID_1635201574">
<node TEXT="The generated html files refer to &quot;styles.css&quot;." ID="ID_425453569"/>
<node TEXT="You also may specify html-css classes for a tree-node, by attaching a &quot;class&quot; meta child-node to it." ID="ID_207438148"/>
</node>
<node TEXT="*** Classes" ID="ID_844918477">
<node TEXT="The application uses &quot;classes&quot; to control the rendering int the website. These are attached to nodes in the input tree by the user or to the output HTML elements by the application." ID="ID_746030578"/>
<node TEXT="Naming of the classes" ID="ID_926174105">
<node TEXT="To avoid name collision with classes possibly introduced by the user or others : Wit uses an own prefix to distinguish : &quot;wit-&quot;. All classes used by wit obey this rule. But in this demo i will forget about it, to avoid repetition. But you should remember that name &quot;x&quot; is really &quot;wit-x&quot; in reality." ID="ID_16104744"/>
<node TEXT="One exception of this rule though exists. The classes &quot;code&quot; and &quot;code-block&quot; work without the wit prefix too, just as with it." ID="ID_1107274921"/>
</node>
<node TEXT="*-&gt; Rendering into HTML" ID="ID_465517052">
<node TEXT="The rendering into HTML is done with HTML classes placed into it that you can use to target by CSS selectors to give custom look to your generated website." ID="ID_1979219677"/>
<node TEXT="Handling the trunk page differently" ID="ID_1588326529">
<node TEXT="You may want to give different style for the trunk page than for other pages. To enable this some elements in the trunk page [and only it] have class &quot;trunk-page&quot;." ID="ID_950094359"/>
<node TEXT="These elements currently are :" ID="ID_551847935">
<node TEXT="The &quot;html&quot; element." ID="ID_674092446"/>
<node TEXT="The &quot;nav&quot; element used for navigation." ID="ID_941728629"/>
<node TEXT="The &quot;hr&quot; element, the horizontal line that separates the navigation bar from the rest of the page." ID="ID_1500483351"/>
</node>
<node TEXT="The &apos;html&apos; element of the trunk page and only of it has class &quot;trunk-page&quot;." ID="ID_392130705"/>
</node>
<node TEXT="In each page" ID="ID_1948285534">
<node TEXT="The navigation bar" ID="ID_639315545">
<node TEXT="Inside the navigation bar" ID="ID_1812641929">
<node TEXT="The main content has class &quot;nav-core&quot;." ID="ID_1031133472"/>
<node TEXT="Each item has class &quot;nav-item&quot;." ID="ID_275124717"/>
<node TEXT="The item corresponding to the current page has class &quot;nav-current-page&quot;." ID="ID_509891622"/>
</node>
<node TEXT="The separator of the navigation bar from the rest of the page has class &quot;navigation-separator&quot;." ID="ID_1254387034"/>
</node>
<node TEXT="The core [without the navigation bar] of the body of a page has class &quot;page_main_part&quot;." ID="ID_1429652542"/>
<node TEXT="In each section" ID="ID_771033704">
<node TEXT="The core contant of each section has class &quot;wit-content&quot;." ID="ID_979466149"/>
<node TEXT="Inline elements containing sentences have class &quot;sentence&quot; - when sentencing is ordered." ID="ID_40811153"/>
<node TEXT="Sentences [as defined by the sentencing feature of wit] have class &quot;text_class_sentence&quot;." ID="ID_1383665184"/>
</node>
<node TEXT="The page breaks [paragraphs that are only links to a subpage] have class &quot;page-break&quot;." ID="ID_16482785"/>
</node>
</node>
</node>
<node TEXT="*** Embedding files" ID="ID_1802052607">
<node TEXT="Embedding files, like images into a site is quite common need. For it I recommend putting your files to be embedded into a &quot;file&quot; sub-folder inside the folder where the source mm file is and extend your compile script with an rsync command to copy this folder into your output folder." ID="ID_1104504927"/>
<node TEXT="An example compile script." ID="ID_365185432">
<node TEXT="# links-to" ID="ID_1930717805">
<node TEXT="external" ID="ID_249609914"/>
<node TEXT="a/example-compile.sh" ID="ID_231479667"/>
</node>
</node>
<node TEXT="Then, from the mindmap file you will refer to it as &quot;file/whatever.ext&quot;." ID="ID_540802275"/>
</node>
<node TEXT="*** Inline Element Types" ID="ID_1010709635">
<node TEXT="*** Text" ID="ID_367424141">
<node TEXT="They are simply just entered without any special formatting." ID="ID_1461762866"/>
</node>
<node TEXT="*** Image" ID="ID_531503458">
<node TEXT="Represented by an &quot;# image&quot; node. This node must have exactly 1 chlid, a plain text node containing the path to the image file." ID="ID_1412565511"/>
</node>
<node TEXT="*** Links" ID="ID_1343525210">
<node TEXT="Any inline element may be a link. To make a node link, attach exactly 1 child to it, a &quot;links-to&quot; node. This node should have exactly 2 children:" ID="ID_14035244">
<node TEXT="The type of link: &quot;external&quot; or &quot;internal&quot;." ID="ID_1831456204"/>
<node TEXT="A plain text node containing the address where the link to point to." ID="ID_478712284"/>
</node>
<node TEXT="Internal Address" ID="ID_1718306237">
<node TEXT="An internal address is a user-defined unique name of a node. To give a name to a node, attach an &quot;id&quot; child node to it having exactly 1 child: a plain text node containing the name to give." ID="ID_1686252576"/>
</node>
</node>
</node>
<node TEXT="*** Structural Element Types" ID="ID_1606462910">
<node TEXT="*** Paragraph" ID="ID_507452569">
<node TEXT="Each leaf text node represents a paragraph." ID="ID_924539501"/>
<node TEXT="Several paragraphs can be united into 1 single by putting them under a &quot;# paragraph&quot; node. This may be necessary if you want to make only a portion of a paragraph a link." ID="ID_1093758359"/>
</node>
<node TEXT="*** Section" ID="ID_538371265">
<node TEXT="Paragraphs can be grouped into a section - just put them under a plain text node, whose content will be the title of the section." ID="ID_1928147339"/>
<node TEXT="Section may contain other section. The depth of the nesting is not limited technically." ID="ID_1475926183"/>
</node>
<node TEXT="*** Sub-page" ID="ID_1058600396">
<node TEXT="If you want a subtree to be rendered into a separate page then label it with a &quot;page&quot; class" ID="ID_28676065"/>
</node>
</node>
</node>
<node TEXT="*-&gt; Links do Not Change" POSITION="right" ID="ID_849144975">
<node TEXT="The links to the generated html pages and to their particular elements are stable, which means that normal editing of your work will not invalidate the links to parts of the generated html website." ID="ID_464000657"/>
<node TEXT="Explanation" ID="ID_25152412">
<node TEXT="The compiler reads from the mindmap file not only the text content of the nodes, but also the unique identifiers of them too, which are generated by the mindmap editor. While the mindmap is edited, the identifiers of the nodes stay the same. And the links are created by the compiler using only these node identifiers." ID="ID_1250791057"/>
</node>
</node>
<node TEXT="*-&gt; Else" POSITION="right" ID="ID_1955835950">
<node TEXT="*** Backward compatibility" ID="ID_686064638">
<node TEXT="Backward compatibility is to be held only for the main tree file input language." ID="ID_989633931">
<node TEXT="But nothing is guaranteed or even promised." ID="ID_785646744"/>
</node>
<node TEXT="But not for :" ID="ID_823674020">
<node TEXT="The command line syntax." ID="ID_602527922"/>
<node TEXT="Your CSS style sheet." ID="ID_127762183"/>
</node>
</node>
<node TEXT="*** Non-changed files keep modification time" ID="ID_933208260">
<node TEXT="The files that do not need a regeneration, because their content did not change - will not be regenerated, so their file modification time will remain as it was. This is handy when you re-upload your website to the server - the file transfer program will need to tansfer only those files which are affected by your edit." ID="ID_1885736636"/>
</node>
<node TEXT="*-&gt; Possible Future Improvements" ID="ID_686015436">
<node TEXT="Inactivate subtree." ID="ID_1980460456">
<node TEXT="An inactivated subtree would not be rendered." ID="ID_23442147"/>
<node TEXT="The language rules would still be enforced inside inactive parts. This is useful to not let inactive parts drop out of instant usability." ID="ID_942830229"/>
</node>
<node TEXT="Tables. Possibly in a general way: with record objects, and visualization function - separating data from visualisation." ID="ID_487781016"/>
<node TEXT="Referring other trees from a tree." ID="ID_1017900896"/>
<node TEXT="The user ability to specify name-value pairs in the command line - as a convinience for parameterized and conditional compilation." ID="ID_1661744885"/>
<node TEXT="Simple programming features: with simple types, structured types, branching, functions. It could even evolve into a general purpose programming language." ID="ID_1039167139"/>
</node>
<node TEXT="This tutorial itself is WrittenInTree." ID="ID_1782879385"/>
</node>
<node TEXT="How to Build from Source" POSITION="right" ID="ID_256227671">
<node TEXT="this is a bit combersome to do and requires some basic technical practice; also: i wrtoe this a long time ago, i only hope that it is uptodate" ID="ID_905619378"/>
<node TEXT="prerequesites" ID="ID_1166095143">
<node TEXT="version control" ID="ID_932652976">
<node TEXT="mercurial" ID="ID_1111862931"/>
</node>
<node TEXT="haskell tools" ID="ID_1144609304">
<node TEXT="compiler : &quot;GHC&quot;" ID="ID_1831848938">
<node TEXT="version &gt;= 8" ID="ID_1117171100"/>
</node>
<node TEXT="package and build manager : &quot;cabal&quot;" ID="ID_1260987876"/>
</node>
</node>
<node TEXT="the application consists of multiple source packages" ID="ID_1516834276">
<node TEXT="some of them are local - these must be handled manually" ID="ID_559083690">
<node TEXT="they are transitively linked together" ID="ID_1685435898"/>
<node TEXT="all the packages are on bitbucket" ID="ID_853939020"/>
<node TEXT="the main package is here" ID="ID_1798971444">
<node TEXT="https://bitbucket.org/libeako/haskell-write-in-tree" ID="ID_1633374249"/>
</node>
<node TEXT="to see the dependencies of a certain local package" ID="ID_547819604">
<node TEXT="look into the package.yaml file of the source of the package" ID="ID_1666921055"/>
<node TEXT="under section &quot;dependencies&quot; there is a group &quot;local&quot;" ID="ID_1898459500">
<node TEXT="the members of this group are the local dependencies" ID="ID_1662904334"/>
</node>
</node>
</node>
</node>
<node TEXT="create an empty folder for the application and do all these instruction inside it" ID="ID_1401719077"/>
<node TEXT="for all local packages, in dependency order" ID="ID_627976102">
<node TEXT="clone the source of it into a separate folder with mercurial" ID="ID_1584218390"/>
<node TEXT="command &apos;cabal sandbox init&apos; to create a cabal sandbox" ID="ID_1352907330"/>
<node TEXT="command &apos;hpack&apos; to generate the cabal project file" ID="ID_220432025"/>
<node TEXT="command &apos;cabal sandbox add-source&apos; the dependencies" ID="ID_1776307803">
<node TEXT="the parameter is the path of the folder of the dependency" ID="ID_1767102654"/>
</node>
<node TEXT="command &apos;cabal --require-sandbox install --dependencies-only&apos; to install the dependencies into this sandbox" ID="ID_647250853"/>
</node>
<node TEXT="in the folder of the main package [write-in-tree]" ID="ID_1033607784">
<node TEXT="command &apos;cabal build&apos;" ID="ID_99108767"/>
<node TEXT="in folder &apos;dist/build/write-in-tree&apos; is the resultant executable" ID="ID_989342293"/>
</node>
</node>
</node>
</map>
