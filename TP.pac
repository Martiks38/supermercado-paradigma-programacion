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
	add: #Fabrica;
	add: #FabricaArticulo;
	add: #Supermercado;
	add: #Usuario;
	add: #Venta;
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\MVP\Presenters\Prompters\Dolphin Choice Prompter'
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

Object subclass: #Fabrica
	instanceVariableNames: ''
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

Fabrica subclass: #FabricaArticulo
	instanceVariableNames: ''
	classVariableNames: ''
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

codigo
	^codigo!

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
codigo!public! !
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

	^distribuidor

	
	! !

!Distribuidor class categoriesForMethods!
createDistribuitor:nombre:comision:zona:!private! !
!

Fabrica guid: (GUID fromString: '{eaf090bb-4d89-47d1-9a04-78717e80d8aa}')!

Fabrica comment: ''!

!Fabrica categoriesForClass!Ejercicio1! !

!Fabrica methodsFor!

crearArticuloComunConCodigo: unCodigo descripcion: unaDescripcion precio: unPrecio
	^'Crear Artículo Común'!

crearArticuloFrioConCodigo: unCodigo descripcion: unaDescripcion precio: unPrecio peso: unPeso
	^'Crear Artículo Frío'.! !

!Fabrica categoriesForMethods!
crearArticuloComunConCodigo:descripcion:precio:!public! !
crearArticuloFrioConCodigo:descripcion:precio:peso:!public! !
!

Supermercado guid: (GUID fromString: '{563120bc-a98d-411e-a648-4dfa26e58384}')!

Supermercado comment: ''!

!Supermercado categoriesForClass!Kernel-Objects! !

!Supermercado methodsFor!

addDistributor

	| distribuidor |

	!

autenticarUsuario

	| estaAutenticado existeUsuario nombre clave |
	
	estaAutenticado := false.
	existeUsuario := false.

	[ estaAutenticado | existeUsuario ] whileFalse: [
		nombre := Prompter prompt: 'Ingrese su nombre de usuario'.
		existeUsuario :=usuarios anySatisfy: [ :u | u nombre = nombre ].
		"existeUsuario ifFalse: [ self registerUserWithName: nombre ].		"
	].

	^nombre.!

buscarDistribuidor!

buscarDistribuidorConCodigo: unCodigo
	^distribuidores detect: [:d | d codigo = unCodigo ]!

crearArticulos: unaFabrica
	
	| unCodigo unaDescripcion unPrecio unPeso tipoArticulo articulo |

	[
		tipoArticulo := ChoicePrompter choices: #('Artículo Común' 'Artícullo Frío').
		tipoArticulo ~= 'C' and: [ tipoArticulo ~= 'F' ].
	] whileTrue.

	[
		unCodigo := Prompter prompt: 'Ingrese el código del artículo'.
		articulos anySatisfy: [:a | a codigo = unCodigo ].
	] whileTrue.

	unaDescripcion := Prompter prompt: 'Ingrese una descripción del artículo'.
	unPrecio := Prompter prompt: 'Ingrese el precio del artículo' asNumber.

	(tipoArticulo = 'C')
		ifTrue: [
				articulo := unaFabrica crearArticuloComunConCodigo: unCodigo descripcion: unaDescripcion precio: unPrecio.
			]
		ifFalse: [
				unPeso := Prompter prompt: 'Ingrese el peso del artículo' asNumber.
				articulo := unaFabrica crearArticuloFrioConCodigo: unCodigo descripcion: unaDescripcion precio: unPrecio peso: unPeso.
			].

	^articulo!

initialize
	
	distribuidores := OrderedCollection new.
	usuarios := OrderedCollection new.
	ventas := OrderedCollection new.
	articulos := OrderedCollection new.

	Usuario initialize.
	Articulo initialize.
	ArticuloFrio initialize.!

menuPrincipal

	| estaAutenticado finalizar userCode |

	userCode := nil.
	finalizar := false.
	estaAutenticado := false.

	Transcript show: 'Supermercado'; cr.

	[
		estaAutenticado ifFalse: [
			userCode := self autenticarUsuario.
			estaAutenticado := true.
		].

		Transcript show: '1. Realizar venta'; cr.
		Transcript show: ''; cr.
		Transcript show: '0. Salir'; cr.

		

		finalizar.
	] whileFalse.!

registerUser
	| unaClave unNombre unDomicilio unaZona user |

	unNombre := Prompter prompt: 'Ingrese el nombre del nuevo usuario'.
	[
		unaClave := Prompter prompt: 'Ingrese la clave única asignada'.
		usuarios anySatisfy: [:u | u clave = unaClave ]
	] whileTrue.
	[
		unaZona := IntegerPrompter prompt: 'Ingrese el número de zona'.
		unaZona <= 0.
	] whileTrue.
	unDomicilio := Prompter prompt: 'Ingrese el domicilio'.
	
	user := Usuario createUserWithKey: unaClave nombre: unNombre domicilio: unDomicilio zona: unaZona.
	
	usuarios add: user.! !

!Supermercado categoriesForMethods!
addDistributor!private! !
autenticarUsuario!public! !
buscarDistribuidor!public! !
buscarDistribuidorConCodigo:!public! !
crearArticulos:!public! !
initialize!private! !
menuPrincipal!public! !
registerUser!public! !
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

clave
	^clave!

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
clave!public! !
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

	[ Codigo isNil ] ifTrue: [ Codigo := 0 ].
	Codigo := Codigo + 1.
	
	usuario := self new.
	usuario clave: unaClave.
	usuario nombre: unNombre.
	usuario domicilio: unDomicilio.
	usuario zona: unaZona.
	usuario codigoUsuario: Codigo.

	^usuario.!

initialize
	Codigo := 0.!

mostrarCodigo
	^Codigo.! !

!Usuario class categoriesForMethods!
createUserWithKey:nombre:domicilio:zona:!private! !
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

autenticarUsuarioConCodigo: unCodigo clave: unaClave
	!

calcularTotal

	| total |
	
	total := 0.
	articulosCantidad do: [ :item | total := total + item articulo getPrice * item cantidad ].
	
	^total!

distribuidor: unDistribuidor
	distribuidor := unDistribuidor!

fecha: unaFecha
	fecha := unaFecha!

usuario: unUsuario
	usuario := unUsuario! !

!Venta categoriesForMethods!
agregarArticuloCantidad:cantidad:!private! !
articulosCantidad:!accessing!private! !
autenticarUsuarioConCodigo:clave:!public! !
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

ArticuloComun guid: (GUID fromString: '{e603849e-bdc4-4b99-8e6b-bd43e0036bf7}')!

ArticuloComun comment: ''!

!ArticuloComun categoriesForClass!Kernel-Objects! !

ArticuloFrio guid: (GUID fromString: '{cfe86318-2aaa-4a3a-b9dd-6d9bb204d9c7}')!

ArticuloFrio comment: ''!

!ArticuloFrio categoriesForClass!Kernel-Objects! !

!ArticuloFrio methodsFor!

getPrice
	"Correción de error de punto flotante"
	^((precio + ( Porcentaje * peso )) * 100 ) rounded / 100 asFloat!

peso: unPeso
	peso := unPeso! !

!ArticuloFrio categoriesForMethods!
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

FabricaArticulo guid: (GUID fromString: '{7f0efa47-ad45-4054-abe6-61050e0aac95}')!

FabricaArticulo comment: ''!

!FabricaArticulo categoriesForClass!Ejercicio1! !

!FabricaArticulo methodsFor!

crearArticuloComunConCodigo: unCodigo descripcion: unaDescripcion precio: unPrecio
	^ArticuloComun createArticleWithCode: unCodigo precio: unPrecio descripcion: unaDescripcion.!

crearArticuloFrioConCodigo: unCodigo descripcion: unaDescripcion precio: unPrecio peso: unPeso
	^ArticuloFrio createArticleWithCode: unCodigo precio: unPrecio descripcion: unaDescripcion peso: unPeso.! !

!FabricaArticulo categoriesForMethods!
crearArticuloComunConCodigo:descripcion:precio:!public! !
crearArticuloFrioConCodigo:descripcion:precio:peso:!public! !
!

"Binary Globals"!

