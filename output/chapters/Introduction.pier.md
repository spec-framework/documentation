

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
First the 3 pillars of 
*Spec* will be explained\. 
Second we expose how 
*Spec* reuse subwidgets and how to customize widgets to make them fit the user needs\.
Third the API of the 
*Spec* basic models is detailled as well as how to understand the meta information attached to this API\. 
The fourth section covers the two dynamic aspects of 
*Spec*\. 
The final section is dedicated to the creation of your own 
*Spec* model\.




    Note: JF: check structure.

