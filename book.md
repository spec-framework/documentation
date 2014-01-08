

#Spec: a new framework for describing user intefaces


##Introduction



The following article will introduce you how to use `Spec` through several examples\. The general purpose of `Spec` will be explained then will follow a presentation of every `Spec` widgets and how to use them\.This paper will be concluded by `n` examples about how to compose basic widgets in order to build complete user interfaces\.

## The heart of Spec


Spec is built around three axes borrowed from the MVP pattern\.Those axes are materialized as three methods: `initializeWidgets`, `initializePresenter`, and `defaultSpec`\.


### `initializeWidgets` the Presenter




### `initializePresenter` the Interactor	




### `defaultSpec` the View


## Where to find what I want


## Spec the Dynamic


Having an user interface with a well known number of sub widgets and a static layout should now sounds easy\. But an user interface is often more than just that\. There are two situations where you need more\. First it happens that the layout of the user interface need to be changed at runtime to match the execution context\.Second if sub widgets are added or removed at runtime\. Then the programmer need to be able to parametrize those new sub widgets on the fly\.


### Dynamic modification of the layout


Changing a widget layout at runtime is quite easy\.It consists of three steps: creating the new layout, setting the needed flag, and build the widget again with the newly created layout\.

The snippet [1\.1\. ](#rebuildDynamically) shows how to simply rebuild a widget with a new layout\.



<a name="rebuildDynamically"></a>**Rebuild a widget at run time**

```smalltalk 
rebuildWithNewLayout
	| newLayout |

	newLayout := self newLayoutCreatedDynamically.
	self needRebuild: false. "tells the interpreter to keep my current ui element"
	self buildWithSpecLayout: newLayout. "rebuilds me with the new layout"
```



One widget can also keep the ui elements of its sub widgets which did not need to be rebuilt\.The message `needRebuild: false` need to be sent to any of those sub widgets\.


### Dynamic add and removal of subwidgets


## Writing my own Spec



### The Model



### The Adapter



### The Bindings
