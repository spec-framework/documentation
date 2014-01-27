

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
Finally a protocol editor is made by reusing a protocol viewer with a text zone in addition\.



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
    		newColumn: [ :r |
    			r 
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
