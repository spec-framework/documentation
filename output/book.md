

#Spec: a new framework for describing user interfaces


##1\. Introduction


Writing user interfaces is notoriously a tedious task\.
It often requires time and a clear understanding of the separation of concerns between the logic of the model and the logic of the user interface\.
Indeed, many frameworks mix domain models with widget models\.


*Spec* is a framework for describing user interfaces\. 
It allows for the separation of concerns between the different parts of the user interface as expressed in the MVP pattern, on which it is inspired\.
*Spec* emphasizes the reuse of widgets as well as their customization\.
The goal of this text is to provide an overview of the functionalities of 
*Spec*\. 


To avoid possible misunderstandings due to confusion in terminology, we first define the following four terms, which will be used frequently:

<dl><dt>UI Element
</dt><dd>an interactive graphical element displayed as part of the Graphical User Interface.</dd><dt>UI Model
</dt><dd>an object that contains the state and behavior of one or several UI elements.</dd><dt>Widget
</dt><dd>the union of a UI Element and its UI model.</dd><dt>Basic widgets
</dt><dd>low level widgets like a list, a button, etc. They are not composed of other widgets.</dd></dl>

The structure of this text is as follows:
First we give a short tutorial on how to define and reuse UIs with 
*Spec*\.
Then the three axes that form the heart of 
*Spec* are explained\. 
In section four the API of the 
*Spec* basic models is detailled as well as how to understand the meta information attached to this API\. 
The next section presents the support for dynamic UIs in 
*Spec*\. 
The sixth section is dedicated to the creation of new 
*Spec* basic widgets, and in section seven the core interpreter loop of 
*Spec* is outlined\.


\(This documentation was written by Benjamin Van Ryseghem and Johan Fabry\. For corrections, comments and questions about this text please send a mail to either of them\.\)


##2\.  Defining and Reusing UIs with *Spec*
<a name="sec_reuse_spec"></a>

This section introduces an example of how to define and reuse 
*Spec* user interfaces\. The example UI that is built will serve to browse the public API of all the basic widgets offered by 
*Spec*\.
This API is further documented in 
[4](#sec_where_to_find_what_I_want)\.
In this section we do not detail the different parts of 
*Spec* yet, for a more in\-depth discussion on the heart of 
*Spec*  we refer to 
[3](#sec_heart_of_spec)\.


The example is structured in four parts\.
First, a list UI named  
**ModelList** that is dedicated to render the subclasses of the 
*AbstractWidgetModel* class is created\.
Second, a UI composed of a list and a label is defined and named 
**ProtocolList**\.
Third, a protocol viewer is defined by combining a 
**ModelList** with two 
**ProtocolList** to browse the 
*protocol* and 
*protocol\-events* methods\.
Finally a protocol browser is made by reusing a protocol viewer and adding a text zone\.



###2\.1\.  The ModelList


Creating a specific UI always starts with the subclassing of 
**ComposableModel**\.
Each sub widget is stored into an instance variable of the newly created class\.
The snippet 
[2\.1](#ex_model_list) shows the definition of this ModelList class\.




<a name="ex_model_list"></a>**ModelList definition**


    ComposableModel subclass: #ModelList
    	instanceVariableNames: 'list'
    	classVariableNames: ''
    	category: 'Spec-Examples'



The first required step then is to instantiate and define the sub widgets\.
This step is done in the method 
`initializeWidgets` as shown in the code 
[2\.2](#ex_modelList_initializeWidgets)\. It creates the list and populates it with the required classes, in alphabetical order\.
More details on the use of the 
`initializeWidgets` method are given in 
[3\.1](#subsec_initializeWidgets)\.




<a name="ex_modelList_initializeWidgets"></a>**Implementation of ModelList>>\#initializeWidgets**


    initializeWidgets
    
    	list := self newList.
    	
    	list items: (AbstractWidgetModel allSubclasses 
    		sorted: [:a :b | a name < b name ]).
    		
    	self focusOrder add: list



The second required step is to define a layout, which is done on the class side\.
Since there is here only one sub widget, the layout is quite simple, as shown in the code in 
[2\.3](#ex_modelList_layout)\.
It simply returns a layout that contains only the list\.
More details on the use of this method are given in 
[3\.3](#subsec_layout)\.




<a name="ex_modelList_layout"></a>**ModelList layout**


    ModelList class>>#defaultSpec
    	<spec: #default>
    	
    	^ SpecLayout composed
    		add: #list;
    		yourself



The three last methods to define on ModelList are a getter, a method to display the UI title and a method to register to list selection changes\.
The code 
[2\.4](#ex_modelList_others) shows the implementation of these three methods and their protocols\.




<a name="ex_modelList_others"></a>**ModelList other methods**


    "accessing"
    list
    	^ list
    	
    "protocol"
    title
    
    	^ 'Widgets'
    	
    "protocol-events"
    whenSelectedItemChanged: aBlock
    
    	list whenSelectedItemChanged: aBlock




The first UI is now done\.
The result can be seen by executing the following snippet of code: 
`ModelList new openWithSpec`\.



###2\.2\.  The ProtocolList


The next user interface is the protocol list\.
This UI combines two sub widgets: a list and a label\.
The class definition is similar to the code above, as can be seen in 
[2\.5](#ex_protocolList_definition)\.




<a name="ex_protocolList_definition"></a>**ProtocolList definition**


    ComposableModel subclass: #ProtocolList
    	instanceVariableNames: 'label protocols'
    	classVariableNames: ''
    	category: 'Spec-Examples'



The 
`initializeWidgets` method for this UI is quite similar to the method in ModelList, as the code in 
[2\.6](#ex_protocolList_init) shows\.




<a name="ex_protocolList_init"></a>**ProtocolList implementation of initializeWidgets**


    initializeWidgets
    
    	protocols := self newList.
    	label := self newLabel.
    	
    	label text: 'Protocol'.
    	protocols	displayBlock: [ :m | m selector ].
    	
    	self focusOrder add: protocols



The layout method is quite different though\.
Now the sub widgets need to be placed more specifically than in the previous example\.
The code 
[2\.7](#ex_protocolList_layout) shows how to build a column with the label on top and the list taking all the space that is left\.




<a name="ex_protocolList_layout"></a>**ProtocolList layout**


    defaultSpec
    	<spec: #default>
    
    	^ SpecLayout composed
    		newColumn: [ :column |
    			column
    				add: #label
    				height: self toolbarHeight;
    				add: #protocols ];
    		yourself



The remaining methods are getters, sub widget delegation methods, a method to display the title, and a method to register to list selection changes\.
The code 
[2\.8](#ex_protocolList_others) shows the implementations of these methods as well as their protocol\.




<a name="ex_protocolList_others"></a>**ProtocolList other methods**


    "accessing"
    label
    	^ label
    	
    "accessing"
    protocols
    	^ protocols
    	
    "protocol"
    items: aCollection
    
    	protocols items: aCollection
    	
    "protocol"
    label: aText
    
    	label text: aText
    	
    "protocol"
    resetSelection
    
    	protocols resetSelection
    	
    "protocol"
    title
    
    	^ 'Protocol widget'
    	
    "protocol-events"
    whenSelectedItemChanged: aBlock
    
    	protocols whenSelectedItemChanged: aBlock



The 
**ProtocolList** UI can be seen by evaluating 
`ProtocolList new openWithSpec`\.



###2\.3\.  The ProtocolViewer


The third user interface is a composition of the two previous user interfaces\.
It is composed of a 
**ModelList** and two 
**ProtocolList**\.
When a model class is selected, the methods in the protocol 
*protocol* and in the protocol 
*protocol\-events* are listed\.


The class has now three instance variables: 
`models` to store the 
**ModelList**, 
`protocols` to store the 
**ProtocolList** for the protocol 
*protocol*, and 
`events` to store the 
**ProtocolList** for protocol 
*protocol\-events*\.
The code in 
[2\.9](#ex_viewer_definition) shows the definition of the class 
**ProtocolViewer**\.




<a name="ex_viewer_definition"></a>**ProtocolViewer definition**


    ComposableModel subclass: #ProtocolViewer
    	instanceVariableNames: 'models protocols events'
    	classVariableNames: ''
    	category: 'Spec-Examples'



The 
`initializeWidgets` method now uses a different way to initialize the sub\-widgets of the UI\. 
This is because it does not use basic widgets but instead reuses the user interfaces we defines previously\.
The remainder of the method is quite similar to the previous implementation, as shown in the code in 
[2\.10](#ex_viewer_initializeWidgets)\.




<a name="ex_viewer_initializeWidgets"></a>**Implementation of ProtocolViewer>>\#initializeWidgets**


    initializeWidgets
    
    	models := self instantiate: ModelList.
    	protocols := self instantiate: ProtocolList.
    	events := self instantiate: ProtocolList.
    	
    	protocols	label: 'protocol'.
    	events label: 'protocol-events'.
    		
    	self focusOrder 
    		add: models;
    		add: protocols;
    		add: events



The layout puts the sub widgets in one column, with all sub widgets taking the same amount of space\.
The code in 
[2\.11](#ex_viewer_layout) shows the implementation of this layout\.




<a name="ex_viewer_layout"></a>**ProtocolViewer column layout**


    defaultSpec
    	<spec: #default>
    	
    	^ SpecLayout composed
    		newColumn: [ :column |
    			column 
    				add: #models; 
    				add: #protocols; 
    				add: #events ];
    		yourself



To describe the interactions between the sub widgets, the method 
`initializePresenter` needs to be defined\.
Here, it specifies that when a class is selected, the selections in the protocol list are reset and both protocol lists are populated\.
Additionally, when a method is selected in one protocol list, the selection in the other list is reset\.
The implementation of this method is exposed in code 
[2\.12](#ex_viewer_presenter)\.
More details on the 
`initializePresenter` method are given in 
[3\.2](#subsec_initializePresenter)\.




<a name="ex_viewer_presenter"></a>**ProtocolViewer sub widget interactions**


    initializePresenter
    
    	models whenSelectedItemChanged: [ :class |
    		self resetProtocolSelection.
    		self resetEventSelection.
    		class
    			ifNil: [ 
    				protocols items: #().
    				events items: #() ]
    			ifNotNil: [ 
    				protocols items: (self methodsIn: class for: 'protocol').
    				events items: (self methodsIn: class for: 'protocol-events') ] ].
    	
    	protocols whenSelectedItemChanged: [ :method | method ifNotNil: [ self resetEventSelection ] ].
    	events whenSelectedItemChanged: [ :method | method ifNotNil: [ self resetProtocolSelection ] ].



The remaining methods are getters, methods to delegate to sub widgets, one method to compute the methods in a specific class for a specific protocol, and methods to register to sub widget events\.
Those methods are given in the code in 
[2\.13](#ex_viewer_others)\.




<a name="ex_viewer_others"></a>**ProtocolViewer other methods**


    "accessing"
    events
    	^ events
    
    "accessing"
    models
    	^ models
    
    "accessing"
    protocols
    	^ protocols
    
    "private"
    methodsIn: class for: protocol
    
    	^ (class methodsInProtocol: protocol)
    		sorted: [ :a :b | a selector < b selector ]
    
    "protocol"
    resetEventSelection
    
    	events resetSelection
    
    "protocol"
    resetProtocolSelection
    
    	protocols resetSelection
    
    "protocol"
    title
    
    	^ 'Protocol viewer'
    
    "protocol-events"
    whenClassChanged: aBlock
    
    	models whenSelectedItemChanged: aBlock
    
    "protocol-events"
    whenEventChangedDo: aBlock
    
    	events whenSelectedItemChanged: aBlock
    
    "protocol-events"
    whenProtocolChangedDo: aBlock
    
    	protocols whenSelectedItemChanged: aBlock



As previously, the result can be seen by executing the following snippet of code: 
`ProtocolViewer new openWithSpec`\.



###2\.4\.  Protocol Editor
<a name="subsec_protocol_editor"></a>

The last user interface reuses a 
**ProtocolViewer** with a different layout and adds a text zone to edit the source code of the selected method\.
The class definition can be seen in code in 
[2\.14](#ex_browser_definition)\.




<a name="ex_browser_definition"></a>**ProtocolBrowser definition**


    ComposableModel subclass: #ProtocolEditor
    	instanceVariableNames: 'viewer text'
    	classVariableNames: ''
    	category: 'Spec-Examples'



The 
`initializeWidgets` implementation is shown in the code in 
[2\.15](#ex_browser_initializeWidgets)\.




<a name="ex_browser_initializeWidgets"></a>**ProtocolEditor>>\#initializeWidgets**


    initializeWidgets
    
    	text := self newText.
    	viewer := self instantiate: ProtocolViewer.
    	
    	text aboutToStyle: true.
    
    	self focusOrder 
    		add: viewer;
    		add: text



The layout is more complex than the previous layouts\.
Now the user interface mainly lays out widgets that are contained in its 
`viewer` sub widget \(the list of models and the two protocol browsers\)\.
The layout is based on a column whose first row is divided in columns\.
The implementation of this method is shown in code in 
[2\.16](#ex_browser_layout)\.




<a name="ex_browser_layout"></a>**ProtocolBrowser layout**


    defaultSpec
    	<spec: #default>
    	
    	^ SpecLayout composed
    		newColumn: [ :col | 
    			col 
    				newRow: [ :r | 
    					r 
    						add: #(viewer models);
    					 	newColumn: [ :c | 
    							c 
    								add: #(viewer protocols);
    								add: #(viewer events) ] ];
    				add: #text
    		];
    		yourself



The 
`initalizePresenter` method is used to make the text zone react to a selection in the lists\.
When a method is seleted, the text zone updates its contents to show the source code  of the selected method\.
The implementation of this method is detailled in the code in 
[2\.17](#ex_browser_presenter)\.




<a name="ex_browser_presenter"></a>**ProtocolBrowser interactions**


    initializePresenter
    
    	viewer whenClassChanged: [ :class | text behavior: class ].
    
    	viewer whenProtocolChangedDo: [ :item | 
    		item 
    			ifNil: [ text text: '' ]
    			ifNotNil: [ text text: item sourceCode ] ].
    	viewer whenEventChangedDo: [ :item | 
    		item 
    			ifNil: [ text text: '' ]
    			ifNotNil: [ text text: item sourceCode ] ]



The other methods are two getters, a method to set the default size, and a method to set the UI title\.
Their implemenations are detailled in code 
[2\.18](#ex_browser_others)\.




<a name="ex_browser_others"></a>**ProtocolBrowser remaining methods**


    "accessing"
    text
    	^ text
    
    "accessing"
    viewer
    	^ viewer
    
    "protocol"
    initialExtent
    
    	^ 750@600
    
    "protocol"
    title
    
    	^ 'Protocols browser'



This finishes the protocol browser\.
The final user interface can be opened with the following snippet: 
`ProtocolBrowser new openWithSpec`\.
The result can be seen in figure 
[2\.1](#fig_protocol_browser)\.


<a name="fig_protocol_browser"></a>![fig_protocol_browser](figures/Protocol_Browser.png "Protocol Browser")

##3\.  The heart of Spec
<a name="sec_heart_of_spec"></a>

All user interfaces in 
*Spec* are constructed through the composition of existing user interfaces\.
To define a user interface, it is sufficient to define the model of the user interface\.
The UI elements that correspond to this model are instantiated by 
*Spec*, depending on the underlying UI framework\.
It is the composition of this model and these UI elements that makes up the resulting widget that is shown to the user, i\.e\. the resulting user interface\.
Hence, since all UIs are constructed through 
**composition** of other UI's, and it is sufficient to define the 
**model** to define the UI, the root class of all UIs is named 
`ComposableModel`\.
So, to define a new user interface, a subclass of 
`ComposableModel` needs to be created\.


As said above, Spec is inspired by the MVP pattern\.
It is built around three axes that materialize themselves as the following three methods in 
`ComposableModel`: 
`initializeWidgets`, 
`initializePresenter`, and 
`defaultSpec`\.
These methods are hence typically found in the model for each user interface\.
In this section we describe the responsibility for each method, i\.e\. how these three work together to build the overall UI\.



###3\.1\.  the *initializeWidgets* method 
<a name="subsec_initializeWidgets"></a>

This method is used to instantiate the models for the different widgets that will be part of the UI and store them in their respective instance variables\.
Instantiation of the models will in turn result in the instantiation and initialization of the different widgets that make up the UI\.
Consequently, configuration and default values of each widget are specified here as well, which is why this method is called 
**initializeWidgets**\.
This focus in this method is to specify what the widgets will look like and what their self\-contained behavior is\.
The behavior to update model state, e\.g\. when pressing a 
`Save` button, is described in this method as well\.
It is explicitly 
**not** the responsibility of this method to define the interactions 
**between** the widgets\.


In general the 
`initializeWidgets` method should follow the pattern:



-  widgets instantiation
-  widgets configuration specification
-  specification of order of focus


The last step is not mandatory but 
**highly** recommended\.
Indeed, without this final step keyboard navigation will not work at all\.


The code in figure 
[3\.1](#pattern) shows an example of an 
`initializeWidgets` method\.
It first instantiates a button and a list widget, storing each in an instance variable\.
It second configures the button it by setting its label\.
Third it specifies the focus order of all the widgets: first the button and then the list\.




<a name="pattern"></a>**Example of initializeWidgets**


    initializeWidgets
    
    	theButton := self newButton.
    	theList := self newList.
    
    	theButton label: 'I am a button'.
    	
    	self focusOrder
    		add: theButton;
    		add: theList.



&nbsp;


    Specifying this method is mandatory, as without it the UI would have no widgets.




####3\.1\.1\.  Widget instantiation


The instantiation of the model for a widget \(and hence the widget\) can be done in two ways: through the use of an creation method or through the use of the 
`instantiate:` method\.
Considering the first option, the framework provides unary messages for the creation of all basic widgets\.
The format of these messages is 
`new[Widget]`, for example 
`newButton` creates a button widget, and 
`newList` creates a list widget, as we have seen above\.
The complete list of available widget creation methods can be found in the class 
*ComposableModel* in the protocol 
*widgets*\.
Considering the second option, to reuse any composite widgets, i\.e\. a subclass of 
*ComposableModel*, the widget needs to be initialized using the 
`instantiate:` method\.
For example, to reuse a 
*MessageBrowser*  widget, the code is 
` self instantiate: MessageBrowser.`


###3\.2\.  The *initializePresenter* method
<a name="subsec_initializePresenter"></a>

This method takes care of the interactions between the different widgets\.
By linking the behavior of the different widgets it specifies the overall presentation, i\.e\. how the overall UI responds to interactions by the user\.


Usually this method consists of specifications of actions to perform when a certain event is received by a widget\.
From the propagation of those events the whole interaction flow of the UI emerges\.
In  
**Spec**, the different UI models are contained in value holders, and the event mechanism relies on the announcements of these value holders to manage the interactions between widgets\.
Value holders provide a single method 
`whenChangedDo:` that is used to register a block to perform on change\.
In addition to this primitive  
`whenChangedDo:` method, the basic widgets provide more specific hooks, e\.g\. when an item in a list is selected or deselected\.


The example 
[3\.2](#ex_button) shows how to use one of the registration methods of the list widget to change the label of the button according to the selection in the list\.




<a name="ex_button"></a>**How to change a button label according to a list selection**


    theList whenSelectedItemChanged: [ :item | 
    	item 
    		ifNil: [ theButton text: 'No selected item' ]
    		ifNotNil: [ theButton text: 'An item is selected'] ]



The whole event API of the basic widgets is described in the section 
[4](#sec_where_to_find_what_I_want)\.





    If a programmer wants his or her widgets to be reused,
    they should provide a comprehensive API.



&nbsp;


    This method is optional. Without it, the different widgets in the UI will simply not respond to changes in each others' state.




###3\.3\.  the *layout* method
<a name="subsec_layout"></a>

This method specifies the layout of the different widgets in the UI\.
It also specifies how a widget reacts when the window is resized\.


For the same UI multiple layouts can be described, and when the UI is built a specific layout to use can be specified\.
If no such specific layout is given, the following lookup mechanism will be used to obtain the layout method:


1.  Search on class side, throughout the whole class hierarchy, for a method with the pragma `<spec: #default>`\.
2.  If multiple such methods exist, the first one found is used\.
3.  If none such methods exist and if there is exactly one method with the pragma `<spec>`, this method is used\.
4.  No layout method is found, an error is raised\.


This method is on class side because it returns a value that usually is the same for all the instances\.
Put differently, usually all the instances of the same user interface have the same layout and hence this can be considered as being a class\-side accessor for a class variable\.
Note that the lookup for the spec method to use starts on instance side, which allows a UI to have a more specific layout depending on the state of the instance\.


The simpliest example of such a method is laying out just one widget\.
Example 
[3\.3](#ex_layout1) presents such a layout\.
It returns a layout in which just one widget is added: the widget contained in 
`theList` instance variable\.




<a name="ex_layout1"></a>**Layout with only one widget**


    ^ SpecLayout composed
    	add: #theList;
    	yourself



The symbol 
`theList` refers to an instance side method returning a widget\.
This is because instance variables are private, so the layout class needs to use an accessor to obtain it when building the UI\.
Note that by default, a widget will take all the space available in its container\.


This method is 
*not* restricted to laying out sub widgets\.
It can also refer to sub widgets contained in sub widgets, i\.e\. when
reusing an existing UI, specify a new layout for the sub widgets that
comprise this UI\.
To do this, instead of giving a symbol, an array with 2 symbols must
be given\.
The first symbol identifies the UI being reused and the second the sub
widget within this UI whose new layout position is being specified\.
We have seen an example of this reuse in 
[2\.4](#subsec_protocol_editor)\.


As said above, multiple layouts can be described for the same user interface\.
In order to retrieve the correct method to apply, these methods need to be flagged with a pragma\.
The pragma can be either 
`<spec: default>` for the layout to use by default, or 
`<spec>` for the other layouts\.





    Specifying this method is mandatory, as without it the UI would show no widgets to the user.





####3\.3\.1\.  Layout Examples


As layouts can become quite complex, this section provides a list of examples of the construction of layouts\.
First two examples are given of the use of 
[rows and columns](#layout_rows_and_column_layout)\.
This is followed by two examples that explain how to set a 
[fixed size](#layout_set_size_pixels) for rows and columns\.
Next is an example that explains how to specify a widget 
[proportionally](#layout_percentage)\.
The last example presents the 
[expert](#layout_expert) mode in case everything else fails\.
To conclude, this section ends with a little 
[explanation](#layout_specify_layout) of how to specify which layout to use when a model defines multiple layouts\.





    All the methods for adding sub widgets can be found in the ''commands'' and ''commands-advanced'' protocols of ""SpecLayout"".


<a name="layout_rows_and_column_layout"></a>
Often the layout of user interfaces can be described in rows and columns, and 
**Spec** provides for an easy way to specify such layouts\.
The example 
[3\.4](#ex_layout_row) shows how to build a row of widgets\.




<a name="ex_layout_row"></a>**Row of widgets**


    ^ SpecLayout composed
    	newRow: [ :row |
    		row
    			add: #theList;
    			add: #theButton
    	];
    	yourself



Having the widgets rendered as a column is similar, as shown in the example 
[3\.5](#ex_layout_column)



<a name="ex_layout_column"></a>**Column of widgets**


    ^ SpecLayout composed
    	newColumn: [ :column |
    		column
    			add: #theList;
    			add: #theButton
    	];
    	yourself




Rows and columns can be combined to build more complex layouts, and splitters between cells can be added\.
The example 
[3\.6](#ex_three_columns) shows how to create a 3 columns layout, containing three buttons in each column\.
This example also shows the 
`addSplitter` message, which adds a splitter between the element added before it and the element added after\.




<a name="ex_three_columns"></a>**3\-column layout**


    ^ SpecLayout composed
    	newRow: [ :row |
    		row
    			newColumn: [ :column | 
    				 column
    				 	add: #button1;
    					add: #button2;
    					add: #button3 
    			];
    			addSplitter;
    			newColumn: [ :column | 
    				 column
    				 	add: #button4;
    					add: #button5;
    					add: #button6 
    			];
    			addSplitter;
    			newColumn: [ :column | 
    				 column
    				 	add: #button7;
    					add: #button8;
    					add: #button9 
    			];
    	];
    	yourself




---

<a name="layout_set_size_pixels"></a>
The height of rows as well as the width of columns can be specified, to prevent them to take all the available space\.
The example 
[3\.7](#ex_row_height) shows how to specify the height of a row in pixels while the example 
[3\.8](#ex_column_width) shows how to specify the column width\.




<a name="ex_row_height"></a>**Row of 30 pixels**


    ^ SpecLayout composed
    	newRow: [ :row |
    		row
    			add: #theList;
    			add: #theButton
    	] height: 30;
    	yourself



&nbsp;

<a name="ex_column_width"></a>**Column of 30 pixels**


    ^ SpecLayout composed
    	newColumn: [ :column |
    		column
    			add: #theList;
    			add: #theButton
    	] width: 30;
    	yourself



Note that it is generally considered a bad habit to hardcode the size of the widgets\.
Methods are available on 
*ComposableModel* providing sensible default sizes, like the width of a button\.
When specifying custom widget sizes, care should be taken to take in account the current font size\.



---

<a name="layout_percentage"></a>
It is also possible to specify the percentage of the container, e\.g\. the window, that a widget should occupy\.
As a result of this, the widget size will change accordingly when the container is resized\.
To do so, the proportional position of the four sides of a widget can be specified, as shown in the example 
[3\.9](#ex_layout_proportional)\.


For each edge, the proportion indicates at what percentage of the overall container the edge should be placed\.
Zero percent is the container edge, 100 percent is the opposite container edge\.
For example, for the top edge, the percentage is counted from the top down: 0 is the top edge, and 1 is the bottom edge\.




<a name="ex_layout_proportional"></a>**A Button centered in, and half the size of its container**


    ^ SpecLayout composed
    	add: #theButton top: 0.25 bottom: 0.25 left: 0.25 right: 0.25;
    	yourself



Also, the argument can be an integer if the offset has to be a fixed number of pixels\.
The number of pixels should be positive, as it indicates a distance from the corresponding edge, going to the opposite edge\.


---

<a name="layout_expert"></a>
The previous examples should cover most of the cases of layout of widgets\.
For the remaining cases there is a last way to specify a widget by specifying its position\.


The method 
`add: origin: corner: ` of 
`SpecLayout` specifies the layout of a widget, percentage\-wise from the origin point to the corner point\.
These two points represent respectively the top left corner and the bottom right corner of the widget\.
The arguments express a percentage of the container, so these 
<u>must</u> be between 
*0@0* and 
*1@1* \.


In addition to those points, two offsets can be also be specified, using the method 
`add: origin: corner:  offsetOrigin: offsetCorner: `\.
The offsets specify the number of pixels that the origin and the corner should be moved\.


Contrary to the previous way to define layouts, while using 
`add: origin: corner:  offsetOrigin: offsetCorner: ` the offset can be negative\.
The offset expresses the number of pixels from the corresponding corner, in the classical computer graphics coordinate system where the origin is in the top left corner\.
Note that this approach is similar to the ProportionalLayout of 
**Morphic**\.



The example 
[3\.10](#ex_layout_expert) shows how to add a widget as a toolbar\.
It specifies that the widget in the 
`toolbar` instance variable should take all the window width, but should be only 30 pixels in height\.




<a name="ex_layout_expert"></a>**Using expert mode to specify a toolbar**


    ^ SpecLayout composed
    	add: #toolbar origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@30;
    	yourself




---

<a name="layout_specify_layout"></a>

As explained in the section 
[3\.3](#subsec_layout), a UI can have multiple layouts\.
So when the layout of a widget that is composed of multiple sub\-widgets is defined, and this widget contains multiple layout methods that determine the layout of its sub\-widgets, the layout method to use can be specified\.


All the methods seen in the previous examples come with a variant used to specify which selector to use for the layout method\.
By example, for the 
`add:` method the variant is  
`add:withSpec:`\.


For example, consider a widget 
**MyWidget** defining a first layout method 
`firstLayout` as the default layout and another layout method called 
`anotherLayout`\.
The example 
[3\.11](#ex_specify_layout) shows how to add an instance of 
**MyWidget** using its 
`anotherLayout` layout method\.




<a name="ex_specify_layout"></a>**How to specify an alternative layout**


    ^ SpecLayout composed
    	add: #myWidget withSpec: #anotherLayout;
    	yourself



##4\.  Where to find what I want
<a name="sec_where_to_find_what_I_want"></a>

All the 
*Spec* models for basic widgets have an API that is explicitly documented through the use of pragmas\.
This section explains where to find the API of a model and meaning of the meta information that is attached to the API methods\.


Each model contains at least two protocols that group the public API methods\.
The first protocol is named 
**protocol**\.
It gathers all the methods that set or get the different state elements of the model plus the behavioral methods acting directly on these elements\.
The second protocol is named 
**protocol\-events**\.
It gathers all the methods that are used to register to a state change\.


All the meta\-information of public API methods is documented through the use of pragmas that start with 
*api:*\.
There are three types of public API methods: getters, setters and registration methods\.



###4\.1\.  Meta information for getters


The pragma for getters is always 
`<api: #inspect>`\.
For example, the code in 
[4\.1](#ex_api_getter) shows how the 
*action*  method in 
**ButtonModel** is implemented\.




<a name="ex_api_getter"></a>**Implementation of ButtonModel>>\#action**


    action
    	<api: #inspect>
    	"get the block performed when the button is clicked"
    
    	^ actionHolder value




###4\.2\.  Meta information for setters


The pragma for setters is a bit more complex\.
The pattern of the pragma is 
`<api: typeOfState getter: getterSelector registration: registrationMethodSelector>`\.
In this pattern, 
*typeOfState* , 
*getterSelector* and 
*registrationMethodSelector* need to be substituted by the relevant values for this setter\.


The 
*getterSelector* specifies the name of the getter method \(a Symbol\) that returns the state set by this method\.
The 
*registrationMethodSelector* states the name of the method \(a Symbol\) that needs to be used to register to changes in the state\.
*typeOfState*  gives the type of the state being set by this setter\.
The possible types are as follows:



-  \#block indicates a block, 
-  \#boolean indicates a boolean,
-  \#color indicates a Color,
-  \#image indicates a Form,
-  \#integer indicates an integer,
-  \#point indicates a Point,
-  \#string indicates a String,
-  \#st indicates any other type of *Smalltalk* object\.


For example, the code in 
[4\.2](#ex_api_setter) shows how 
*actions:* is implemented in 
**ButtonModel**\.




<a name="ex_api_setter"></a>**Implementation of ButtonModel>>\#action:**


    action: aBlock
    	<api: #block getter: #getAction registration: #whenActionChangedDo:>
    	"set the block performed when the button is clicked"
    
    	actionHolder value: aBlock




###4\.3\.  Meta information for registration methods


The pragma for registration methods information is always 
*<api: \#event>*\.
For example, the code in 
[4\.3](#ex_api_registration) shows how the method 
*whenActionChangedDo:* is implemented in 
**ButtonModel**\.




<a name="ex_api_registration"></a>**Implementation of ButtonModel>>\#whenActionChangedDo**


    whenActionChangedDo: aBlock 
    	<api: #event>
    	"Set the block performed when the action to perform is changed"
    
    	actionHolder whenChangedDo: aBlock




###4\.4\.  Meta information for behavioral methods


The other methods should be mainly methods that implement some
behavior of the widget\.
The pragma for these methods is 
*<api: \#do>*\.
For example,  
[4\.4](#ex_resetSelection) shows how 
*resetSelection* is implemented in 
**ListModel**\.




<a name="ex_resetSelection"></a>**Implementation of ListModel>>\#resetSelection**


    resetSelection
    	<api: #do>
    	"Unselect every items"
    
    	selectionHolder reset.
    	multiSelectionHolder removeAll



##5\.  Spec the Dynamic
<a name="sec_spec_the_dynamic"></a>

Having an user interface with a well known number of sub widgets and a static layout is not always sufficient\. 
A user interface is often more than just that, for example here are two situations where more is needed: 
First, when the layout of the user interface needs to be changed at runtime to match the execution context of the software\.
Second, sub widgets are added or removed at runtime and therefore the programmer needs to be able to parametrize those new sub widgets on the fly\.


*Spec* also provides support for such dynamic user interfaces\.
In this section we show how to use 
*Spec* in these situations\.


First, we talk about making dynamic modifications of the layout of widgets, and second we discuss the dynamic adding and removing of subwidgets\.
Third and last we show how the dynamic features can be used to quickly prototype a user interface\.



###5\.1\.  Dynamic modification of the layout


Changing the layout of widgets at runtime is straightforward, as we will see here\.
Such changes basically consist of three steps:


1.  creating the new layout,
2.  setting a flag to prohibit the creation of a new UI element \(and instead reuse the existing one\),
3.  building the UI again with the newly created layout\.


The code in 
[5\.1](#rebuildDynamically) is an example of rebuilding a widget with a new layout\.
First, a helper method is used to obtain a 
`SpecLayout` object that determines the new layout\.
Second, the 
`needRebuild` flag is set to 
`false` such that the existing UI element is reused\.
Third, the rebuilding of the user interface is performed\.




<a name="rebuildDynamically"></a>**Rebuild a widget at run time**


    rebuildWithNewLayout
    	| newLayout |
    
    	newLayout := self newLayoutCreatedDynamically.
    	self needRebuild: false. "tells the interpreter to keep my current UI element"
    	self buildWithSpecLayout: newLayout. "rebuilds me with the new layout"



One widget can also keep the UI elements of its sub widgets which do not need to be rebuilt\.
The message 
`needRebuild: false` needs to be sent to any of those sub widgets\.
For example, if a model comprising a 
*button* and a 
*list* just wants to rearrange the position of these UI elements, there is no need to rebuild them, i\.e\. instantiate new UI elements for them\.
To prevent this, the message 
`needRebuild: false` should be send to them, as shown in the example 
[5\.2](#ex_needRebuild)\.




<a name="ex_needRebuild"></a>**How to need rebuild sub widgets**


    rebuildWithNewLayout
    	| newLayout |
    
    	newLayout := self newLayoutCreatedDynamically.
    	self needRebuild: false.
    	theButton needRebuild: false.
    	theList needRebuild: false.
    	self buildWithSpecLayout: newLayout.




###5\.2\.  Dynamic adding and removal of subwidgets


If a user interface needs a varying number of subwidgets, the amount of which cannot be established at compilation time, then another approach is needed\.
In this scenario, 
`DynamicComposableModel` is the model that needs to be subclassed, as this class provides support for the required kind of dynamic behavior\.


Amongst others, this class adds the method 
`assign:to:`, which takes a model instance as a first argument, and a unique symbol as a second argument\.
This method is used to assign an already instantiated model as sub widget, instead of the method 
`instantiateModels:` that takes a class name as argument and instantiates a new model\.


When using 
`DynamicComposableModel`, the instantiation of the sub widgets is a bit different from normal use\.
In the 
`instantiateWidgets` method, instead of instantiating each widget separately, 
`instantiateModels:` should be used to instantiate them\.
This method takes as argument an array of pairs, where each pair is composed of the unique name of the widget as key, and the name of the widget class as value\.
This allows for a widget to be accessed by sending a message whose selector is the widget name to the model\.


By example, if a widget named 
`button` is created, then this widget can be accessed by calling 
`self button` as shown in the example 
[5\.3](#ex_dynamic_creation)\.




<a name="ex_dynamic_creation"></a>**Dynamic creation of a widget**


    self instantiateModels: #( button ButtonModel ).
    	self button label: 'Click me'.




###5\.3\.  Examples: Prototyping a UI


Thanks to the capability of 
*Spec* to dynamically instantiate widgets, it is also possible to prototype a user interface from within any workspace\.
The following examples show how 
*Spec* can be used to quickly prototype a user interace\.


The first example explains how to design by prototyping a user interface\.
The second example introduces the composition of dynamic models\.



####5\.3\.1\.  Designing a pop up


This example shows how to easily and quickly design a popup window asking for an input\.


First we create a simple model with two sub widgets, a label and a text field, as shown by the snippet 
[5\.4](#ex_widget_creation)\.




<a name="ex_widget_creation"></a>**Create a widget**


    view := DynamicComposableModel new
    	instantiateModels: #(label LabelModel text TextInputFieldModel);
    	yourself.



We can then specify the title and the initial size of the widget, adding the code in 
[5\.5](#ex_set_title)\.




<a name="ex_set_title"></a>**Specify the title and the initial size**


    view extent: 300@90;
    	title: 'Choose your project'.



Then we specify the UI element layout\.
It will be only one row with the label and the text field\.
The snippet 
[5\.6](#ex_layout) shows the layout definition\.




<a name="ex_layout"></a>**Define the layout**


    layout := SpecLayout composed
    	newRow: [ :r | r add: #label width: 75; add: #text ];
    	yourself.



To have a first idea of the resulting UI, we can already open it as follows: 
`view openWithSpecLayout: layout`\.
This UI however does not have any sub widget state or behavior so there is not much to see or do at this point\.


The next step is to set up the sub widget state and behavior\.
We set the text of the label as well as the ghost text of the textfield\.
We also specify here that the text field should automatically accept the text on each keystroke, such that it does not show the yellow 'edited' triangle on the top right\.
This is shown in the code in 
[5\.7](#ex_setup_subwidget)\.




<a name="ex_setup_subwidget"></a>**Setting up the sub widgets**


    view label text: 'Packages:'.
    
    view text
    	autoAccept: true;
    	entryCompletion: nil;
    	ghostText: '.*'.



Opening the UI again \(
`view openWithSpecLayout: layout`\) now shows the text of the label and the ghost text of the text field\.


As we want the widget to be a popup with a single button 
*Ok*, the toolbar to use should be defined explicitly \(the default toolbar has an 
*Ok* button and a 
*Cancel* button\)\.
We also set the toolbar action for when 
*Ok* is clicked: the current text of the text field will be saved in the instance variable 
*regex*\.
The code in 
[5\.8](#ex_toolbar) shows how to do it\.




<a name="ex_toolbar"></a>**Instantiating the toolbar**


    toolbar := OkToolbar new
    	okAction: [ regex := view text text ];
    	yourself.



We can also add a shortcut to the text field on 
*Enter* to simulate the click on 
*Ok*\.
The code 
[5\.9](#ex_shortcut) illustrates how to set up such a shortcut\.




<a name="ex_shortcut"></a>**Add a shortcut**


    view text 
    	bindKeyCombination: Character cr asKeyCombination 
    	toAction: [ toolbar triggerOkAction ].



This completes the specification of the UI\.
As a final step, when opening it we pass it the toolbar and configure it to be centered in the Pharo window and modal\.
The code in 
[5\.10](#ex_final) shows the final version of the code




<a name="ex_final"></a>**Open the widget**


    view := DynamicComposableModel new
    	instantiateModels: #(label LabelModel text TextInputFieldModel);
    	extent: 300@90;
    	title: 'Choose your project'
    	yourself.
    	
    view label text: 'Packages:'.
    
    layout := SpecLayout composed
    	newRow: [ :r | r add: #label width: 75; add: #text ];
    	yourself.
    
    view text
    	autoAccept: true;
    	entryCompletion: nil;
    	ghostText: '.*'.
    	
    toolbar := OkToolbar new
    	okAction: [ regex := view text text ];
    	yourself.
    	
    view text 
    	bindKeyCombination: Character cr asKeyCombination 
    	toAction: [ toolbar triggerOkAction ].
    	
    (view openDialogWithSpecLayout: layout)
    	toolbar: toolbar;
    	centered;
    	modalRelativeTo: World.



The result can be seen in Figure 
[5\.1](#fig_popup)\.


<a name="fig_popup"></a>![fig_popup](figures/Popup.png "Prototype of a popup")


####5\.3\.2\.  Composing dynamic models


This exemple shows in three parts how to buid a simple code browser\.


First a simple list widget is created displaying all the subclasses of AstractWidgetModel\.




<a name="ex_dyn_list"></a>**Defining a list widget**


    m := DynamicComposableModel new.
    m instantiateModels: #( list ListModel ).
    m list items: (AbstractWidgetModel allSubclasses sorted: [:a :b | a name < b name ]).
    m layout: (SpecLayout composed
    	add: #list;
    	yourself).
    m openWithSpec.



Then the list widget is reused to build a viewer widget displaying the protocol methods of the selected class\.




<a name="ex_dyn_protocols"></a>**Definition of a protocol methods viewer**


    m2 := DynamicComposableModel new.
    m2 assign: m to: #classes.
    m2 instantiateModels: #( methods ListModel ).
    m list whenSelectedItemChanged: [ :item | 
    	item 
    		ifNil: [ m2 methods: #() ]
    		ifNotNil: [ m2 methods items: ((item selectorsInProtocol: 'protocol') sorted) ] ].
    m2 layout: (SpecLayout composed
    	newRow: [ :r | r add: #classes; add: #methods ];
    	yourself).
    m2 openWithSpec.



Finally the last widget is defined with the previously created viewer\.
In addition, a text zone is added to show the selected methods source code\.




<a name="ex_dyn_browser"></a>**Definition of a Protocol Browser**


    m3 := DynamicComposableModel new.
    m3 assign: m2 to: #top.
    m3 instantiateModels: #( text TextModel ).
    m2 methods whenSelectedItemChanged: [ :selector | 
    	selector ifNil: [ m3 text text: '' ] ifNotNil: [ m3 text text: (m list selectedItem >> selector ) sourceCode ] ].
    m3 layout: (SpecLayout composed
    	newColumn: [ :c | c add: #top; add: #text ];
    	yourself).
    	
    m3 openWithSpec.
    m3 title: 'Protocol browser'



The final result looks like the Figure 
[5\.2](#ex_browser)\.


<a name="ex_browser"></a>![ex_browser](figures/Dyn_Protocol_Browser.png "Prototype of Protocol Browser")

##6\.  Creating new basic widgets


*Spec* provides for a large amount and wide variety of basic widgets\. 
In the rare case that a basic widget is missing, the 
*Spec* framework will need to be extended to add this new widget\.
In this section we will explain how to create such a new basic widget\.


We will first explain the applicable part of how the widget build process is performed\.
This will reveal the different actors in the process and provide a clearer understanding of their responsibilities\.
We then present the three steps of widget creation: writing a new model, writing an adapter, and updating or creating an individual UI framework binding\.



###6\.1\.  One step in the building process of a widget


The UI building process does not make a distinction between basic and composed widgets\.
At a specific point in the building process of a basic widget the default spec method of the widget is called, just as if it would be a composed widget\.
However in this case, instead of providing a layout for multiple widgets that comprise the UI, this method builds an adapter to the underlying UI framework\.
Depending of the underlying UI framework that is currently used, this method can provide different kind of adapters, for example an adapter for Morphic, or an adapter for Seaside, etc\.


The adapter, when instantiated by the UI model, will in turn instantiate a widget that is specific to the UI framework being used\.


For example, when using a List in the Morphic UI, the adaptor will be a MorphicListAdapter and it will contain a PluggableListMorph\.
This is this framework\-specific widget that will be added to the widget container\.


Figure 
[6\.1](#model_adapter_uielement) shows the relationship between those objects\.


<a name="model_adapter_uielement"></a>![model_adapter_uielement](figures/Model-Adapter-UIElement.png "Relationship between the model, the adapter, and the UI element")


###6\.2\.  The Model


The new model needs to be a subclass of 
**AbstractWidgetModel** and its name should be composed of the new basic widget concept, e\.g\. list or button, and of the word 
*Model*\.
The responsibility of the model is to store all the state of the widget\.
Examples of widget\-specific state are:


-  the index of a list
-  the label of a button
-  the action block for when a text is validated in a text field


The state is wrapped in value holders and kept in instance variables\.
For example, the code in 
[6\.1](#ex_value_holder) shows how to wrap the state 
`0` in a value holder and keep it as an instance variable\.
Value holders are needed because they are later used to propagate state changes and thus create the interaction flow of the user interface, as discussed in Section 
[3](#sec_heart_of_spec)\.




<a name="ex_value_holder"></a>**Storing state wrapped in a Value Holder in an instance variable**


    index := 0 asValueHolder.



For each instance variable that holds state three methods should be defined: the getter, the setter, and the registration method\.
The first two should be classified in the protocol named 
*protocol* while the registration method should be in 
*protocol\-events*\.
For example, the code in 
[6\.2](#ex_mutators) shows the methods for the example code in 
[6\.1](#ex_value_holder)\. 




<a name="ex_mutators"></a>**Example of mutators for index**


    "in protocol: protocol"
    index
    	^index value
    
    "in protocol: protocol"
    index: anInteger
    	index value: anInteger
    
    "in protocol: protocol-events"
    whenIndexChanged: aBlock
    	index whenChangedDo: aBlock



The last step to define a new model is to implement a method 
`adapterName` on the class side\.
The method should be in the protocol named 
*spec* and should return a symbol\.
The symbol should be composed of the basic concept of the widget, e\.g\. list or button, and the word 
*Adapter*, like 
**ListAdapter**\.


The communication from the UI model to the adapter is performed using the dependents mechanism\.
This mechanism is used to to handle the fact that a same model can have multiple UI elements concurrently displayed\.
The message 
`changed: with: ` is used to send the message 
*selector* with the arguments 
*aCollection* to the adapters\.
Each adapter can then convert this 
*Spec* message into a framework specific message\. 
For example, the method 
`#filterWith:` sent by 
**TreeModel** via 
`changed: with:` is implemented as shown in 
[6\.3](#ex_filter_with) in MorphicTreeAdapter




<a name="ex_filter_with"></a>**Implementation of MorphicTreeAdapter>>\#filterWith:**


    filterWith: aFilter
    	
    	self widgetDo: [ :w || nodes |
    		nodes := w model rootNodes.
    		nodes do: [:r | r nodeModel updateAccordingTo: aFilter].
    	
    		self removeRootsSuchAs: [:n | (aFilter keepTreeNode: n) not and: [n isEmpty]].
    
    		self changed: #rootNodes ].




###6\.3\.  The Adapter


An adapter must be a subclass of 
**AbstractAdapter**\.
The adapter name should be composed of the UI framework name, e\.g\. Morphic, and the name of the adapter it is implementing, e\.g\. ListAdapter\.
The adapter is an object used to connect a UI framework specific element to the framework independent model\.


The only mandatory method for an adapter is 
`defaultSpec` on the class side\.
This method has the responsibility to instantiate the corresponding UI element\.


The example 
[6\.4](#ex_adapter_instanciation) shows how 
**MorphicButtonAdapter** instantiates its UI element\.




<a name="ex_adapter_instanciation"></a>**How MorphicButtonAdapter instantiates its UI element**


    defaultSpec
    	<spec>
    	
    	^ #(PluggableButtonMorph
    			#color:								#(model color)
    	    	#on:getState:action:label:menu: 	#model #state #action #label nil
    			#getEnabledSelector: 				#enabled
    			#getMenuSelector:					#menu:
    			#hResizing: 						#spaceFill
    			#vResizing: 						#spaceFill
    			#borderWidth:						#(model borderWidth)
    			#borderColor:						#(model borderColor)
    			#askBeforeChanging:					#(model askBeforeChanging)
    			#setBalloonText:					#(model help)
    			#dragEnabled:						#(model dragEnabled)
    			#dropEnabled:						#(model dropEnabled)	
    			#eventHandler:						#(EventHandler on:send:to: keyStroke keyStroke:fromMorph: model))



Since the adapter is bridging the gap between the element of the UI framework and the model, the adapter also needs to forward the queries from the UI element to the model\.
Seen from the other way around: since the model is holding the state, the adapter is used to update the UI element state of the model\.


The methods involved in the communication from the model to the adapter as well as the methods involved in the communication from the adapter to the UI model should be in the protocol 
*spec protocol*\.
On the other hand the methods involved in the communication from the adapter to the UI element and vice versa should be categorized in the protocol 
*widget API*\.


To communicate with the UI element, the adapter methods use the method 
`widgetDo:`\.
This method executes the block provided as argument, which will only happen after the UI element has already been created\.


The example 
[6\.5](#ex_emphasis) shows how 
**MorphicLabelAdapter** propagates the modification of the emphasis from the adapter to the UI element\.




<a name="ex_emphasis"></a>**How MorphicLabelAdapter propagates the emphasis changes**


    emphasis: aTextEmphasis
    
    	self widgetDo: [ :w | w emphasis: aTextEmphasis ]




###6\.4\.  The UI Framework binding


The binding is an object that is used to resolve the name of the adapter at run time\.
This allows for the same model to be used with several UI frameworks\.


Adding the new adapter to the default binding is quite simple\.
It requires to update two methods: 
`initializeBindings` in 
**SpecAdapterBindings** and 
`initializeBindings` in the framework specific adapter class, e\.g\. 
**MorphicAdapterBindings** for Morphic\.


The method 
`SpecAdapterBindings>>#initializeBindings` is present only to expose the whole set of adapters required\.
It fills a dictionary, as shown in the code 
[6\.6](#ex_adapter_init)\.




<a name="ex_adapter_init"></a>**Implementation of SpecAdapterBindings>>\#initializeBindings**


    initializeBindings
    	"This implementation is stupid, but it exposes all the containers which need to be bound"
    	
    	bindings
    		at: #ButtonAdapter				put: #ButtonAdapter;
    		at: #CheckBoxAdapter			put: #CheckBoxAdapter;
    		at: #ContainerAdapter			put: #ContainerAdapter;
    		at: #DiffAdapter				put: #MorphicDiffAdapter;
    		at: #ImageAdapter				put: #ImageAdapter;
    		at: #LabelAdapter				put: #LabelAdapter;
    		at: #ListAdapter				put: #ListAdapter;
    		at: #IconListAdapter			put: #IconListAdapter;
    		at: #DropListAdapter			put: #DropListAdapter;
    		at: #MultiColumnListAdapter		put: #MultiColumnListAdapter;
    		at: #MenuAdapter				put: #MenuAdapter;
    		at: #MenuGroupAdapter			put: #MenuGroupAdapter;
    		at: #MenuItemAdapter			put: #MenuItemAdapter;	
    		at: #NewListAdapter				put: #NewListAdapter;
    		at: #RadioButtonAdapter			put: #RadioButtonAdapter;
    		at: #SliderAdapter				put: #SliderAdapter;
    		at: #TabManagerAdapter			put: #TabManagerAdapter;
    		at: #TabAdapter					put: #TabAdapter;
    		at: #TextAdapter				put: #TextAdapter;
    		at: #TextInputFieldAdapter		put: #TextInputFieldAdapter;
    		at: #TreeAdapter				put: #TreeAdapter;
    		at: #TreeColumnAdapter			put: #TreeColumnAdapter;
    		at: #TreeNodeAdapter			put: #TreeNodeAdapter;		
    		at: #WindowAdapter				put: #WindowAdapter;
    		at: #DialogWindowAdapter		put: #DialogWindowAdapter;
    		yourself



Each UI framework\-specific adapter set should define its own bindings\.
To implement a new binding, a subclass of 
**SpecAdapterBindings** must be defined that overrides the method 
`initializeBindings`\.
This method must bind 
*Spec* adapter names with framework specific adapter class names\.
The example 
[6\.7](#ex_morphic_bindings) shows how the morphic binding implements the method 
`initializeBindings`\.




<a name="ex_morphic_bindings"></a>**Definition of Morphic specific bindings**


    initializeBindings
    	
    	bindings
    		at: #ButtonAdapter				put: #MorphicButtonAdapter;
    		at: #CheckBoxAdapter			put: #MorphicCheckBoxAdapter;
    		at: #ContainerAdapter			put: #MorphicContainerAdapter;
    		at: #DiffAdapter				put: #MorphicDiffAdapter;
    		at: #DropListAdapter			put: #MorphicDropListAdapter;
    		at: #LabelAdapter				put: #MorphicLabelAdapter;
    		at: #ListAdapter				put: #MorphicListAdapter;
    		at: #IconListAdapter			put: #MorphicIconListAdapter;
    		at: #ImageAdapter				put: #MorphicImageAdapter;
    		at: #MultiColumnListAdapter		put: #MorphicMultiColumnListAdapter;
    		at: #MenuAdapter				put: #MorphicMenuAdapter;
    		at: #MenuGroupAdapter			put: #MorphicMenuGroupAdapter;
    		at: #MenuItemAdapter			put: #MorphicMenuItemAdapter;
    		at: #NewListAdapter				put: #MorphicNewListAdapter;
    		at: #RadioButtonAdapter			put: #MorphicRadioButtonAdapter;
    		at: #SliderAdapter				put: #MorphicSliderAdapter;
    		at: #TabManagerAdapter			put: #MorphicTabManagerAdapter;
    		at: #TabAdapter					put: #MorphicTabAdapter;
    		at: #TextAdapter				put: #MorphicTextAdapter;
    		at: #TextInputFieldAdapter		put: #MorphicTextInputFieldAdapter;
    		at: #TreeAdapter				put: #MorphicTreeAdapter;
    		at: #TreeColumnAdapter			put: #MorphicTreeColumnAdapter;
    		at: #TreeNodeAdapter			put: #MorphicTreeNodeAdapter;
    		at: #WindowAdapter				put: #MorphicWindowAdapter;
    		at: #DialogWindowAdapter		put: #MorphicDialogWindowAdapter;
    		yourself



Once this is done, the bindings should be re\-initialized by running the following snippet of code: 
`SpecInterpreter hardResetBindings`\.


Then during the process of computing a 
*Spec* model and its layout into a framework specific UI element, the binding can be changed to change the output framework\.
The binding is managed by the 
**SpecInterpreter**\.


The example 
[6\.8](#ex_setting_bindings) shows how to do change the binding to use the 
**MyOwnBindingClass** class\.




<a name="ex_setting_bindings"></a>**How to set custom bindings**


    SpecInterpreter bindings: MyOwnBindingClass new.



Note that the 
**SpecInterpreter** bindings are reset after each execution\.


##7\.  The Spec interpreter
<a name="sec_spec_interpreter"></a>

In order to create a framework specific UI element a 
*Spec* model is interpreted via the 
**SpecInterpreter**\.
The interpreter is in charge of recursively interpreting all the sub widgets of a model\.
It then combines them, according to the given layout and the given binding\.


The interpreter entry point is the method 
`SpecInterpreter>>#interpretASpec:selector:`\.


This section will explain the two main parts of the interpretation of a model: data collection and the interpretation loop\.
In addition, the flow diagram 
[7\.1](#fig_flow_diagram) outlines the different steps of the process and their relationships\.


<a name="fig_flow_diagram"></a>![fig_flow_diagram](figures/Interpretation_Chart.png "Spec interpretation flow chart")


###7\.1\.  Collect the data


Before the interpretation loop itself, the interpreter starts by collecting the required data from the model\.
The first element is the array to interpret\.
The second is the spec wrapper used to encapsulate data during the interpretation\.


The array to interpret is extracted from the layout provided by the model\.
The code 
[7\.1](#ex_extract_array) shows the conversion of a SpecLayout into an array of literals that the interpreter can iterate over\.




<a name="ex_extract_array"></a>**Conversion of a SpecLayout into an Array of literals**


    SpecLayout composed
    	newRow: [ :row | row add: #theList ];
    	asArray
    	 
    "returns"	 
    
    #(#ContainerModel 
    	#add: #(
    		#(#ContainerModel 
    			#add: #(
    				#(#model #theList) 
    					#layout: #(
    						#SpecLayoutFrame 
    							#leftFraction: 0 
    							#topFraction: 0 
    							#rightFraction: 1 
    							#bottomFraction: 1 
    							#leftOffset: 0 
    							#topOffset: 0 
    							#rightOffset: 0 
    							#bottomOffset: 0
    					)
    			)
    		) #layout: #(
    			#SpecLayoutFrame 
    				#leftFraction: 0 
    				#topFraction: 0 
    				#rightFraction: 1 
    				#bottomFraction: 1 
    				#leftOffset: 0 
    				#topOffset: 0 
    				#rightOffset: 0 
    				#bottomOffset: 0
    		)
    	)
    )



During the recursive calls of the interpretation loop, the interpreter can be called with any kind of object\.
So the first step is the extraction of the data to interpret, if any\.
If there is no data to interpret, it means that the recursive calls reached a 
*primitive object* \(like the integer in the example 
[7\.1](#ex_extract_array)\), i\.e\. an object which can not be interpreted\.
In this case the primitive object is directly returned\.


The second item to collect, if the first step succeeded, is the spec wrapper that will be used throughout the interpretation\.
This step checks if the model provided as argument needs to be rebuild or not\.
If not, the currently existing UI element is directly returned\.
Otherwise this step results in the creation of a wrapper object that keeps the current model as well as the receiver of messages that will be performed in the interpretation loop\.
The result of the interpretation is this wrapper object, and the type of the wrapper is determined by the first element of the array\.
For example, the type of the wrapper that results from the interpretation of the example 
[7\.1](#ex_extract_array) is determined by the very first 
`#ContainerModel` element\.



###7\.2\.  Interpretation loop


Once all the required data is collected, the interpretation loop can begin\.


The loop is quite simple\. The first element of the array is popped out of the array to interpret\.
This literal is the selector of the method that will be performed on the receiver that was previously stored inside the wrapper\.
According to the selector, an adequate number of arguments are popped from the array to interpret\.


Each argument is then interpreted using a new 
**SpecInterpreter** instance, following the exact same process\.
This step is the step leading to recursive calls\.


The selector is then performed on the current receiver and given as arguments the results of the argument interpretation\.


The loop ends when the array is empty\.
The resulting adapter is returned and linked to the model that was provided to the interpreter\.
