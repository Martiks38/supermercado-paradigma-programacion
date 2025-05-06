| package |
package := Package name: 'TP'.
package paxVersion: 1;
	basicComment: ''.

package classNames
	add: #Articulo;
	add: #ArticuloCantidad;
	add: #ArticuloComun;
	add: #ArticuloFrio;
	add: #Distribuidor;
	add: #Supermercado;
	add: #Usuario;
	add: #Venta;
	add: #Zona;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Integer Prompter'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Prompter').

package!

"Class Definitions"!

Object subclass: #Articulo
	instanceVariableNames: 'codigo precio descripcion'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #ArticuloCantidad
	instanceVariableNames: 'articulo cantidad'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Distribuidor
	instanceVariableNames: 'codigo nombre comision zona ventas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Supermercado
	instanceVariableNames: 'distribuidores ventas usuarios articulos zonas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Usuario
	instanceVariableNames: 'codigoUsuario clave nombre domicilio zona'
	classVariableNames: 'Codigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Venta
	instanceVariableNames: 'id articulosCantidad fecha usuario distribuidor'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Zona
	instanceVariableNames: 'distribuidor id'
	classVariableNames: 'Id'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Articulo subclass: #ArticuloComun
	instanceVariableNames: ''
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Articulo subclass: #ArticuloFrio
	instanceVariableNames: 'peso'
	classVariableNames: 'Porcentaje'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

"End of package definition"!

"Source Globals"!

"Classes"!

Articulo guid: (GUID fromString: '{d0532dd0-56e2-4662-a8f6-aa6476d479c6}')!

Articulo comment: ''!

!Articulo categoriesForClass!Kernel-Objects! !

!Articulo methodsFor!

cargarDatos: unCodigo precio: unPrecio descripcion: unaDescripcion 
	codigo:= unCodigo.
	precio:= unPrecio.
	descripcion:= unaDescripcion.!

codigo: unCodigo
	codigo := unCodigo!

descripcion: unaDescripcion
	descripcion := unaDescripcion!

getPrice
	^precio!

precio: unPrecio
	precio := unPrecio! !

!Articulo categoriesForMethods!
cargarDatos:precio:descripcion:!public! !
codigo:!accessing!public! !
descripcion:!accessing!public! !
getPrice!private! !
precio:!accessing!public! !
!

!Articulo class methodsFor!

createArticleWithCode: unCodigo precio: unPrecio descripcion: unaDescripcion

	| articulo |

	articulo := self new.
	articulo precio: unPrecio.
	articulo codigo: unCodigo.
	articulo descripcion: unaDescripcion.

	^articulo.! !

!Articulo class categoriesForMethods!
createArticleWithCode:precio:descripcion:!private! !
!

ArticuloCantidad guid: (GUID fromString: '{deaf4749-c169-40ff-bd3e-269d6ac531dc}')!

ArticuloCantidad comment: ''!

!ArticuloCantidad categoriesForClass!Kernel-Objects! !

!ArticuloCantidad methodsFor!

articulo
	^articulo!

articulo: unArticulo
	articulo := unArticulo!

cantidad
	^cantidad!

cantidad: unaCantidad
	cantidad := unaCantidad! !

!ArticuloCantidad categoriesForMethods!
articulo!private! !
articulo:!accessing!private! !
cantidad!private! !
cantidad:!accessing!private! !
!

!ArticuloCantidad class methodsFor!

createWithArticle: unArticulo cantidad: unaCantidad
	
	| item |
	
	item := self new.

	item articulo: unArticulo.
	item cantidad: unaCantidad.
	
	^item.
	
	!

verificarCantidad: unaCantidad
	^unaCantidad > 0! !

!ArticuloCantidad class categoriesForMethods!
createWithArticle:cantidad:!private! !
verificarCantidad:!private! !
!

Distribuidor guid: (GUID fromString: '{428bb17d-965a-49e5-b076-f037fbacd32b}')!

Distribuidor comment: ''!

!Distribuidor categoriesForClass!Kernel-Objects! !

!Distribuidor methodsFor!

asignarVenta: unaVenta
	ventas add: unaVenta.!

cargarDatos: unCodigo nombre:unNombre  comision:unComision zona: unaZona 

	ventas:= OrderedCollection new!

codigo: unCodigo
	codigo := unCodigo.!

comision: unaComision
	comision := unaComision.!

nombre: unNombre
	nombre := unNombre.!

ventas: listaVentas
	ventas := listaVentas.
	!

zona: unaZona
	zona := unaZona.! !

!Distribuidor categoriesForMethods!
asignarVenta:!private! !
cargarDatos:nombre:comision:zona:!public! !
codigo:!accessing!private! !
comision:!accessing!private! !
nombre:!accessing!private! !
ventas:!private! !
zona:!accessing!private! !
!

!Distribuidor class methodsFor!

createDistribuitor: unCodigo nombre: unNombre comision: unaComision zona: unaZona

	| distribuidor |
	
	distribuidor := self new.
	distribuidor codigo: unCodigo.
	distribuidor zona: unaZona.
	distribuidor nombre: unNombre.
	distribuidor comision: unaComision.
	distribuidor ventas: OrderedCollection new.

	^distribuidor

	
	! !

!Distribuidor class categoriesForMethods!
createDistribuitor:nombre:comision:zona:!private! !
!

Supermercado guid: (GUID fromString: '{563120bc-a98d-411e-a648-4dfa26e58384}')!

Supermercado comment: ''!

!Supermercado categoriesForClass!Kernel-Objects! !

!Supermercado methodsFor!

addDistributor

	| distribuidor |

	!

addZone	
	zonas add: Zona createZone.!

altaArticulo
|codigo precio descripcion esFrio peso articulo|
"
codigo:=Prompter prompt:'ingrese codigo'.
precio:= Prompter prompt:'ingrese precio'.
descripcion:= Prompter prompt:'ingrese descripcion'.

esFrio:= Prompter prompt:'Ingrese s-frio n-comun'.
(esFrio = 's') ifTrue:[
	peso:= Prompter prompt:'ingrese peso'.
	articulo:= ArticuloFrio new.
	articulo cargarDatos: codigo precio: precio descripcion: descripcion peso: peso.
] ifFalse:[
	articulo:= ArticuloComun new.
	articulo cargarDatos: codigo precio: precio descripcion: descripcion.
].

articulos add:articulo.
"


!

altaDistribuidor!

altaUsuario!

altaVenta!

autenticarse!

buscarDistribuidor!

crearZona!

initialize
	
	distribuidores := OrderedCollection new.
	usuarios := OrderedCollection new.
	ventas := OrderedCollection new.
	articulos := OrderedCollection new.
	zonas := OrderedCollection new.

	Usuario initialize.
	Zona initialize.
	ArticuloFrio initialize.!

registerUser
	| unaClave unNombre unDomicilio unaZona user |

	unNombre := Prompter prompt: 'Ingrese el nombre del nuevo usuario'.
	unaClave := Prompter prompt: 'Ingrese la clave'. "Supongo que las claves serán únicas y siempre se ingresarán correctamente"
	unDomicilio := Prompter prompt: 'Ingrese el domicilio'.
	[ 
		unaZona := IntegerPrompter prompt: 'Ingrese el código de la zona'.
		self validateZone: unaZona.
	] whileFalse.
	
	user := Usuario createUser: unaClave nombre: unNombre domicilio: unDomicilio zona: unaZona.
	
	usuarios add: user.!

validateZone: unaZona
	^zonas includes: unaZona.! !

!Supermercado categoriesForMethods!
addDistributor!private! !
addZone!private! !
altaArticulo!public! !
altaDistribuidor!public! !
altaUsuario!public! !
altaVenta!public! !
autenticarse!public! !
buscarDistribuidor!public! !
crearZona!public! !
initialize!private! !
registerUser!private! !
validateZone:!private! !
!

!Supermercado class methodsFor!

new
	^self basicNew initialize.! !

!Supermercado class categoriesForMethods!
new!private! !
!

Usuario guid: (GUID fromString: '{07653269-a79b-4701-abb8-c21aa98982a4}')!

Usuario comment: ''!

!Usuario categoriesForClass!Kernel-Objects! !

!Usuario methodsFor!

clave: unaClave
	clave := unaClave!

codigoUsuario: unCodigoUsuario
	codigoUsuario := unCodigoUsuario!

domicilio!

domicilio: unDomicilio
	domicilio := unDomicilio!

nombre!

nombre: unNombre
	nombre := unNombre!

zona: unaZona
	zona := unaZona! !

!Usuario categoriesForMethods!
clave:!accessing!private! !
codigoUsuario:!accessing!private! !
domicilio!public! !
domicilio:!accessing!private! !
nombre!public! !
nombre:!accessing!private! !
zona:!accessing!private! !
!

!Usuario class methodsFor!

createUserWithKey: unaClave nombre: unNombre domicilio: unDomicilio zona: unaZona

	| usuario |

	Codigo := Codigo + 1.
	
	usuario := self new.
	usuario clave: unaClave.
	usuario nombre: unNombre.
	usuario domicilio: unDomicilio.
	usuario zona: unaZona.
	usuario codigoUsuario: Codigo.

	^usuario.!

inicializar
Codigo := 0.!

initialize
	Codigo := 0.!

mostrarCodigo
	^Codigo.! !

!Usuario class categoriesForMethods!
createUserWithKey:nombre:domicilio:zona:!private! !
inicializar!public! !
initialize!private! !
mostrarCodigo!public! !
!

Venta guid: (GUID fromString: '{e18476b6-4dc8-46ba-91f6-7655502ea2b9}')!

Venta comment: ''!

!Venta categoriesForClass!Kernel-Objects! !

!Venta methodsFor!

agregarArticuloCantidad: unArticulo cantidad: unaCantidad

	| item |
	item := ArticuloCantidad createWithArticle: unArticulo cantidad: unaCantidad.
	articulosCantidad add: item.!

articulosCantidad: listaItems
	articulosCantidad := listaItems!

calcularTotal
	^articulosCantidad inject: 0 into: [:acc :cur | cur articulo getPrice * cur cantidad ]!

distribuidor: unDistribuidor
	distribuidor := unDistribuidor!

fecha: unaFecha
	fecha := unaFecha!

usuario: unUsuario
	usuario := unUsuario! !

!Venta categoriesForMethods!
agregarArticuloCantidad:cantidad:!private! !
articulosCantidad:!accessing!private! !
calcularTotal!private! !
distribuidor:!accessing!public! !
fecha:!accessing!private! !
usuario:!accessing!public! !
!

!Venta class methodsFor!

createSaleWithCode: unCodigo usuario: unUsuario distribuidor: unDistribuidor
	
	| venta |

	venta := self new.
	venta codigo: unCodigo.
	venta usuario: unUsuario.
	venta distribuidor: unDistribuidor.
	venta articulosCantidad: OrderedCollection new.
	venta fecha: Date today.

	^venta.! !

!Venta class categoriesForMethods!
createSaleWithCode:usuario:distribuidor:!private! !
!

Zona guid: (GUID fromString: '{7cb4607d-b831-49fd-a126-18843f49e39b}')!

Zona comment: ''!

!Zona categoriesForClass!Kernel-Objects! !

!Zona methodsFor!

distribuidor: unDistribuidor
	distribuidor := unDistribuidor.!

id: unId
	id := unId.! !

!Zona categoriesForMethods!
distribuidor:!private! !
id:!private! !
!

!Zona class methodsFor!

createZone
	| zone |

	Id := Id + 1.

	zone := self new.
	zone id: Id.

	^zone.!

initialize
	Id := 0.! !

!Zona class categoriesForMethods!
createZone!private! !
initialize!private! !
!

ArticuloComun guid: (GUID fromString: '{e603849e-bdc4-4b99-8e6b-bd43e0036bf7}')!

ArticuloComun comment: ''!

!ArticuloComun categoriesForClass!Kernel-Objects! !

ArticuloFrio guid: (GUID fromString: '{cfe86318-2aaa-4a3a-b9dd-6d9bb204d9c7}')!

ArticuloFrio comment: ''!

!ArticuloFrio categoriesForClass!Kernel-Objects! !

!ArticuloFrio methodsFor!

cargarDatos: unCodigo precio: unPrecio descripcion: unaDescripcion peso: unPeso
	super cargarDatos: unCodigo precio: unPrecio descripcion: unaDescripcion.

	peso:= unPeso.!

getPrice
	"Correción de error de punto flotante"
	^((precio + ( Porcentaje * peso )) * 100 ) rounded / 100 asFloat!

peso: unPeso
	peso := unPeso! !

!ArticuloFrio categoriesForMethods!
cargarDatos:precio:descripcion:peso:!public! !
getPrice!private! !
peso:!accessing!private! !
!

!ArticuloFrio class methodsFor!

createArticleWithCode: unCodigo precio: unPrecio descripcion: unaDescripcion peso: unPeso
	
	| articulo |
	
	articulo := super createArticleWithCode: unCodigo precio: unPrecio descripcion: unaDescripcion.
	articulo peso: unPeso.
	
	^articulo!

inicializar
	Porcentaje := (Prompter prompt: 'Ingrese su porcentaje para todos sus articulos frios' ) asNumber.!

initialize
	[
		Porcentaje := ( Prompter prompt: 'Ingrese el porcentaje de incremento de los productos fríos' ) asNumber.
		Porcentaje < 0.
	] whileTrue.
	!

porcentaje
	^Porcentaje.! !

!ArticuloFrio class categoriesForMethods!
createArticleWithCode:precio:descripcion:peso:!private! !
inicializar!public! !
initialize!private! !
porcentaje!private! !
!

"Binary Globals"!

