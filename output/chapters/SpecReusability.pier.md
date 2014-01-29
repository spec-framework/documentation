

##1\.  Reuse with *Spec*
<a name="sec_reuse_spec"></a>

The section introduces an exemple of how to define and reuse 
*Spec* models\.


The example is structured in four parts\.
First a list dedicated to render the subclasses of 
`AbstractWidgetModel` is created under the name 
**ModelList**\.
Second a UI composed of a list and a label is defined and named 
**ProtocolList**\.
Third a protocol viewer is defined by combining a 
**ModelList** with two 
**ProtocolList** to browse the 
*protocol* and 
*protocol\-events* methods\.
Finally a protocol browser is made by reusing a protocol viewer with a text zone in addition\.



###1\.1\.  The MethodList


Defining a dedicated list is quite simple since 
**ListModel** provides default values for all the states\.
Creating a specific widget always starts with the subclassing of 
**ComposableModel**\.
Each sub widget is stored into an instance variable\.
The snippet 
[1\.1](#ex_model_list) shows the definition of ModelList\.




<a name="ex_model_list"></a>**ModelList definition**


    ComposableModel subclass: #ModelList
    	instanceVariableNames: 'list'
    	classVariableNames: ''
    	category: 'Spec-Examples'



The first required step then is to instantiate and define the sub widget\.
This step is done in the method 
**MethodList** as shown is the code 
[1\.2](#ex_modelList_initializeWidgets)\.




<a name="ex_modelList_initializeWidgets"></a>**Implementation of ModelList>>\#initializeWidgets**


    initializeWidgets
    
    	list := self newList.
    	
    	list items: (AbstractWidgetModel allSubclasses 
    		sorted: [:a :b | a name < b name ]).
    		
    	self focusOrder add: list



The second required step is to define a layout on class side\.
Since there is here only one sub widget, the layout is quite simple as shown in code 
[1\.3](#ex_modelList_layout)\.




<a name="ex_modelList_layout"></a>**ModelList layout**


    ModelList class>>#defaultSpec
    	<spec: #default>
    	
    	^ SpecLayout composed
    		add: #list;
    		yourself



The three last methods defined on MethodList are a getter, and method to display the UI title and a method to register to list selection changes\.
The code 
[1\.4](#ex_modelList_others) shows the implementation of these three methods and their protocol\.




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
The result can be seen by performing the following snippet: 
`ModelList new openWithSpec`\.



###1\.2\.  The ProtocolList


The next user interface is the protocol list\.
This UI combines two sub widgets: a list and a label\.
The class definition is then similar to the code 
[1\.5](#ex_protocolList_definition)\.




<a name="ex_protocolList_definition"></a>**ProtocolList definition**


    ComposableModel subclass: #ProtocolList
    	instanceVariableNames: 'label protocols'
    	classVariableNames: ''
    	category: 'Spec-Examples'



The 
`initializeWidgets` method for this UI is quite similar of the ModelList method as the code 
[1\.6](#ex_protocolList_init) shows\.




<a name="ex_protocolList_init"></a>**ProtocolList implementation of initializeWidgets**


    initializeWidgets
    
    	protocols := self newList.
    	label := self newLabel.
    	
    	label text: 'Protocol'.
    	
    	self focusOrder add: protocols



The layout method is quite different though\.
Indeed the sub widgets need to be placed more specifically than previously\.
The code 
[1\.7](#ex_protocolList_layout) shows how to build a column with the label on top and the list taking all the space left\.




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



The remaining methods are getters, sub widget delegation methods, a method to disply the title, and a method to register to list selection changes\.
The code 
[1\.8](#ex_protocolList_others) shows the implementations of these methods as well as their protocol\.




<a name="ex_protocolList_others"></a>**ProtocolList other methods**


    "accessing"
    label
    	^ label
    	
    "accessing"
    protocols
    	^ protocols
    	
    "protocol"
    displayBlock: aBlock
    
    	protocols displayBlock: aBlock
    
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
**ProtocolList** UI can be seen by evaluating the snippet 
`ProtocolList new openWithSpec`\.



###1\.3\.  The ProtocolViewer


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
The code 
[1\.9](#ex_viewer_definition) shows the definition of the class 
**ProtocolViewer**\.




<a name="ex_viewer_definition"></a>**ProtocolViewer definition**


    ComposableModel subclass: #ProtocolViewer
    	instanceVariableNames: 'models protocols events'
    	classVariableNames: ''
    	category: 'Spec-Examples'



The 
`initializeWidgets` method remains quite similar to the previous implementation as shown in the code 
[1\.10](#ex_viewer_initializeWidgets)\.




<a name="ex_viewer_initializeWidgets"></a>**Implementation of ProtocolViewer>>\#initializeWidgets**


    initializeWidgets
    
    	models := self instantiate: ModelList.
    	protocols := self instantiate: ProtocolList.
    	events := self instantiate: ProtocolList.
    	
    	protocols
    		label: 'protocol';
    		displayBlock: [ :m | m selector ].
    	events
    		label: 'protocol-events';
    		displayBlock: [ :m | m selector ].
    		
    	self focusOrder 
    		add: models;
    		add: protocols;
    		add: events



The layout put the sub widget in one column, with all sub widget taking the same amount of space\.
The code 
[1\.11](#ex_viewer_layout) shows the implementation of this layout\.




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



The method 
`initializePresenter` describe the interactions between the sub widgets\.
Here it specifies that when a class is selected, the two protocol lists are populated\.
The protocol list selections are also resetted when a new class is selected\.
The implementation of this method is exposed in code 
[1\.12](#ex_viewer_presenter)\.




<a name="ex_viewer_presenter"></a>**ProtocolViewer interactions**


    initializePresenter
    
    	models whenSelectedItemChanged: [ :class |
    		protocols resetSelection.
    		events resetSelection.
    		class
    			ifNil: [ 
    				protocols items: #().
    				events items: #() ]
    			ifNotNil: [ 
    				protocols items: (self methodsIn: class for: 'protocol').
    				events items: (self methodsIn: class for: 'protocol-events') ] ].
    	
    	protocols whenSelectedItemChanged: [ :method | method ifNotNil: [ self resetEventSelection ] ].
    	events whenSelectedItemChanged: [ :method | method ifNotNil: [ self resetProtocolSelection ] ].



The remaining methods are getters, methods to delegate to sub widgets, one method to compute the methods in a specific class for a specific protocol, and methods to register to sub widgets events\.
Those methods are summed up in the code 
[1\.13](#ex_viewer_others)\.




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



As previously the result can be seen by executing the following snippet: 
`ProtocolViewer new openWithSpec`\.



###1\.4\.  Protocol Editor


The last user interface reuses a 
**ProtocolViewer** with a different and a text zone to visualize the selected method sourceCode\.
The class definition can be seen in code 
[1\.14](#ex_browser_definition)\.




<a name="ex_browser_definition"></a>**ProtocolBrowser definition**


    ComposableModel subclass: #ProtocolEditor
    	instanceVariableNames: 'viewer text'
    	classVariableNames: ''
    	category: 'Spec-Examples'



The 
`initializeWidgets` implementation is exposed in code 
[1\.15](#ex_browser_initializeWidgets)\.




<a name="ex_browser_initializeWidgets"></a>**ProtocolEditor>>\#initializeWidgets**


    initializeWidgets
    
    	text := self newText.
    	viewer := self instantiate: ProtocolViewer.
    	
    	text aboutToStyle: true.
    
    	self focusOrder 
    		add: viewer;
    		add: text



The layout is more complex than previous layouts\.
Indeed, the user interface mainly layout sub widget sub widgets\.
The layout is based on a column which first row is divided in columns\.
The implementation of this method is shown in code 
[1\.16](#ex_browser_layout)\.




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
`initalizePresenter` method is used to make the text zone reacts to lists selection\.
When a method is seleted, the text zone update its contents to show the selected method source code\.
The implementation of this method is detailled in code 
[1\.17](#ex_browser_presenter)\.




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



The others methods are two getters, a method to set the default size, and a method to set the UI title\.
Their implemenations are detailled in code 
[1\.18](#ex_browser_others)\.




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



The final user interface can be opened with the following snippet: 
`ProtocolBrowser new openWithSpec`\.
The result can be seen in figure 
[1\.1](#fig_protocol_browser)\.


<a name="fig_protocol_browser"></a>![fig_protocol_browser](figures/Protocol_Browser.png "Protocol Browser")