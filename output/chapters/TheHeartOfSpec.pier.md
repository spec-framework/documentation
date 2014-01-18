

##1\.  The heart of Spec
<a name="sec_heart_of_spec"></a>

Spec is built around three axes that are inspired by the MVP pattern\.
These axes materialize themselves as the following three methods: 
`initializeWidgets`, 
`initializePresenter`, and 
`defaultSpec`\.



    Note: For JF we need to talk about the name of the class, you need to subclass it to make a new UI

&nbsp;


    Note: For JF add some blah of the interplay/how the 3 work together to build the overall UI and we discuss the role of the 3 methods here


We first detail some necessary terminology before discussing each of these methods in more detail\.



To avoid possible misunderstandings in this text due to confusion in terminology, we define four terms:

<dl><dt>UI Element
</dt><dd>an interactive graphical element displayed as part of the Graphical User Interface.</dd><dt>UI Model
</dt><dd>an object that contains the state and behavior of one or several UI elements.</dd><dt>Widget
</dt><dd>the union of a UI Element and its UI model.</dd><dt>Basic widgets
</dt><dd>low level widgets like a list, a button, etc. They are not composed of other widgets.</dd></dl>



###1\.1\.  the *initializeWidgets* method  <sub>\(the MVP View\)</sub>


This method is used to instantiate the different widgets that are part of the UI and store them in their respective instance variables\.
The configuration and default values of each widget are specified here as well\.
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
[1\.1](#pattern) shows an example of an 
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




####1\.1\.1\.  Widget instantiation


The instantiation of a widget can be done in two ways: through the use of an creation method or through the use of the 
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


###1\.2\.  The *initializePresenter* method <sub>\(the MVP Interactor\)</sub>


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
[1\.2](#ex_button) shows how to use one of the registration methods of the list widget to change the label of the button according to the selection in the list\.




<a name="ex_button"></a>**How to change a button label according to a list selection**


    theList whenSelectedItemChanged: [ :item | 
    	item 
    		ifNil: [ theButton text: 'No selected item' ]
    		ifNotNil: [ theButton text: 'An item is selected'] ]



The whole event API of the basic widgets is described in the section 
[¿?](#sec_where_to_find_what_I_want)\.





    If a programmer wants his or her widgets to be reused,
    they should provide a comprehensive API.



&nbsp;


    This method is optional. Without it, the different widgets in the UI will simply not respond to changes in each others' state.




###1\.3\.  the *layout* method <sub>\(the MVP Presenter\)</sub>
<a name="subsec_layout"></a>

This method specifies the layout of the different widgets in the UI\.
It also specifies how a widget reacts when the window is resized\.


For the same UI multiple layouts can be described, and when the UI is built a specific layout to use can be specified\.
If no such specific layout is given, the following lookup mechanism will be used to obtain the layout method:


1.  Search on class side, throughout the whole class hierarchy, for a method with the pragma *<spec: \#default>*\.
2.  If multiple such methods exist, the first one found is used\.
3.  If none such methods exist and if there is exactly one method with the pragma *<spec>*, this method is used\.
4.  No layout method is found, an error is raised\.


This method is on class side because it returns a value that usually is the same for all the instances\.
Put differently, usually all the instances of the same user interface have the same layout and hence this can be considered as being a class\-side accessor for a class variable\.
Note that the lookup for the spec method to use starts on instance side, which allows a UI to have a more specific layout depending on the state of the instance\.


The simpliest example of such a method is laying out just one widget\.
The example 
[1\.3](#ex_layout1) presents such a layout\.
It returns a layout in which just one widget is added: the widget contained in 
`theList` instance variable\.




<a name="ex_layout1"></a>**Layout with only one widget**


    ^ SpecLayout composed
    	add: #theList;
    	yourself



The symbol 
`theList` refers to an instance side method returning a widget\.
This is because as instance variables are private, the layout class needs to use an accessor to obtain it when building the UI\.
Note that by default, a widget will take all the space available\.


As said above, multiple layouts can be described for the same user interface\.
In order to retrieve the correct method to apply, these methods need to be flagged with a pragma\.
The pragma can be either 
`<spec: default>` for the layout to use by default, or 
`<spec>` for the other layouts\.





    Specifying this method is mandatory, as without it the UI would show no widgets to the user.





####1\.3\.1\.  Layout Examples


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
[explanation](#layout_specify_layout) of how to specify which layout to use and where to find the complete API\.

<a name="layout_rows_and_column_layout"></a>
Often the layout of user interfaces can be described in rows and columns, and 
**Spec** provides for an easy way to specify such layouts\.
The example 
[1\.4](#ex_layout_row) shows how to build a row of widgets\.




<a name="ex_layout_row"></a>**Row of widgets**


    ^ SpecLayout composed
    	newRow: [ :row |
    		row
    			add: #theList;
    			add: #theButton
    	];
    	yourself



Having the widgets rendered as a column is similar, as shown in the example 
[1\.5](#ex_layout_column)



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
[1\.6](#ex_three_columns) shows how to create a 3 column layout, containing three buttons in each column\.
This example also shows the 
`addSplitter` message, which adds a splitter between the element added before it and the element added after\.




<a name="ex_three_columns"></a>**3 columns layout**


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
[1\.7](#ex_row_height) shows how to specify the height of a row in pixels while the example 
[1\.8](#ex_column_width) how to specify the column width\.




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
[1\.9](#ex_layout_proportional)\.


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
[1\.10](#ex_layout_expert) shows how to add a widget as a toolbar\.
It specifies that the widget in the 
`toolbar` instance variable should take all the window width, but should be only 30 pixels in height\.




<a name="ex_layout_expert"></a>**Using expert mode to specify a toolbar**


    ^ SpecLayout composed
    	add: #toolbar origin: 0@0 corner: 1@0 offsetOrigin: 0@0 offsetCorner: 0@30;
    	yourself




---

<a name="layout_specify_layout"></a>

All the methods for adding sub widgets can be found in the 
*commands* and 
*commands\-advanced* protocols of 
**SpecLayout**\.



As explained in the section 
[1\.3](#subsec_layout), a UI can have multiple different layouts\.
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
[1\.11](#ex_specify_layout) shows how to add an instance of 
**MyWidget** using its 
`anotherLayout` layout method\.




<a name="ex_specify_layout"></a>**How to specify an alternative layout**


    ^ SpecLayout composed
    	add: #myWidget withSpec: #anotherLayout;
    	yourself

