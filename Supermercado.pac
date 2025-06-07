| package |
package := Package name: 'Supermercado'.
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
	yourself.

package binaryGlobalNames: (Set new
	yourself).

package globalAliases: (Set new
	yourself).

package setPrerequisites: #(
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Legacy Date & Time'
	'..\..\Documents\Dolphin Smalltalk 7\Core\Object Arts\Dolphin\Base\Dolphin Message Box'
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
	instanceVariableNames: 'codigo nombre comision zonas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Supermercado
	instanceVariableNames: 'articulos distribuidores usuarios ventas'
	classVariableNames: ''
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Usuario
	instanceVariableNames: 'id nombre clave domicilio zona'
	classVariableNames: 'Codigo'
	poolDictionaries: ''
	classInstanceVariableNames: ''!

Object subclass: #Venta
	instanceVariableNames: 'codigo usuario distribuidor fecha articulosCantidad'
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

"End of package definition"!

"Source Globals"!

"Classes"!

Articulo guid: (GUID fromString: '{8b57ddf9-167b-4204-ae74-fb6dc5d33b32}')!

Articulo comment: ''!

!Articulo categoriesForClass!Ejercicio1! !

!Articulo methodsFor!

cargarDatos
    codigo :=(Prompter prompt: 'Ingrese codigo ') asNumber.
    precio :=(Prompter prompt: 'Ingrese el precio del articulo ') asNumber.
    descripcion:=Prompter prompt:'Ingrese la descripcion del articulo'.!

codigo
^codigo.!

descripcion
^descripcion.!

obtenerPrecio
^precio.!

precio
    ^precio! !

!Articulo categoriesForMethods!
cargarDatos!public! !
codigo!public! !
descripcion!public! !
obtenerPrecio!public! !
precio!public! !
!

ArticuloCantidad guid: (GUID fromString: '{0ab25e77-192a-4eed-b5ec-087c1be39df5}')!

ArticuloCantidad comment: ''!

!ArticuloCantidad categoriesForClass!Ejercicio1! !

!ArticuloCantidad methodsFor!

articulo
    ^articulo!

articulo: unArticulo
    articulo := unArticulo.!

cantidad
    ^cantidad!

cantidad: unaCantidad
    cantidad := unaCantidad.! !

!ArticuloCantidad categoriesForMethods!
articulo!public! !
articulo:!public! !
cantidad!public! !
cantidad:!public! !
!

Distribuidor guid: (GUID fromString: '{6b85e248-a5fc-4b97-af73-89c307541c80}')!

Distribuidor comment: ''!

!Distribuidor categoriesForClass!Ejercicio1! !

!Distribuidor methodsFor!

cargarDatos
|seguir zona|

codigo := (Prompter prompt: 'Ingrese codigo') asNumber.
nombre:= Prompter prompt: 'Ingrese nombre'.
comision:= (Prompter prompt: 'Ingrese la comision de reparto') asNumber.
seguir := true.
[seguir] whileTrue:[
    zona := (Prompter prompt:'Ingrese una zona ').
    zonas add:zona.
    seguir:= MessageBox confirm: 'Desea seguir agregando zonas?'
]!

codigo
^codigo.!

comision
^comision.!

zonas
^zonas.! !

!Distribuidor categoriesForMethods!
cargarDatos!public! !
codigo!public! !
comision!public! !
zonas!public! !
!

Supermercado guid: (GUID fromString: '{b9aff32e-95f1-40a7-bf17-dccd439002d7}')!

Supermercado comment: ''!

!Supermercado categoriesForClass!Ejercicio1! !

!Supermercado methodsFor!

altaArticulo
    |articulo opc|
    opc := Prompter prompt: '1. Articulo frio 2. Articulo comun'.
    (opc = '1') ifTrue: 
        [articulo := ArticuloFrio new.  ] 
    ifFalse:
        [articulo := ArticuloComun new. ].
    "Cargar datos de dicho articulo"
    articulo cargarDatos.

    "Agregar arituclo a coleccion"
    articulos add:  articulo.!

altaDistribuidor
|d|
d := Distribuidor new.

d cargarDatos.

distribuidores add: d.!

altaUsuario
|u|

u:= Usuario new.

u cargarDatos.

usuarios add: u.!

buscarArticulo:unCodigo
^articulos detect: [:a | a codigo =  unCodigo ] ifNone:[nil]!

buscarDistribuidor:unaZona
    ^distribuidores detect: [:d |  (d zonas) anySatisfy: [:z | z = unaZona] ] ifNone:[nil].!

buscarUsuario:unNombre
      ^ usuarios detect:[:u | u nombre  = unNombre] ifNone:[nil].!

inicializar
    distribuidores:= OrderedCollection new.
    ventas:= OrderedCollection new.
    usuarios:= OrderedCollection new.
    articulos := OrderedCollection new.
    ArticuloFrio inicializar.
    Usuario inicializar.!

listarVenta
    ventas do: 
        [:venta | 
            Transcript show: venta codigo.
            Transcript show: venta fecha.
            Transcript show: venta usuario nombre.
            Transcript show: venta distribuidor nombre
        ]!

mostrarArticulos
    articulos do: 
        [:a| 
            Transcript show: a codigo printString; tab; show: a precio printString; tab; show: a descripcion; cr. 
        ]!

registrarVenta: unUsuario
|vent dis seguir art cod item|

"Inicializar la venta"
vent := Venta new.
vent cargarDatos.
vent usuario: unUsuario.

"Buscar distribuidor por zona del usuario; si no hay, cancelar"
dis := self buscarDistribuidor: (unUsuario zona).
(dis isNil) ifTrue: [ ^false ].

"Iniciar ciclo de carga de artículos"
seguir := true.
[seguir] whileTrue: [

    self mostrarArticulos.
    cod := (Prompter prompt: '¿Ingrese el cod del articulo solicitado?') asNumber.
    art := self buscarArticulo: cod.

    "Agregar el ítem a la venta"
    vent agregarArticuloCantidad: art.

    "Consultar si desea seguir agregando artículos"
    seguir := MessageBox confirm: 'Desea seguir?'.
].!

validarUsuario
|seguir usuario  nombre registrar clave|
"Variable de control del bucle"
seguir := true.

"Variable para guardar el usuario identificado"
usuario := nil.

"Repetir hasta que se ingrese un usuario válido y su clave"
[seguir] whileTrue: [

    "Pedir nombre de usuario"
    nombre := Prompter prompt: 'Ingrese su nombre de usuario: '.

    "Buscar el usuario por nombre"
    usuario := self buscarUsuario: nombre.

    "Si no se encuentra el usuario..."
    (usuario isNil)
        ifTrue: [
            MessageBox notify: 'Usted ingresó mal su usuario'.

            "Ofrecer la opción de registrarse"
            registrar := MessageBox confirm: '¿Desea ser registrado?'.

            (registrar = true)
                ifTrue: [
            self altaUsuario .
            "Mostrar mensaje de confirmación"
            MessageBox notify: 'A continuación se le volverán a solicitar los datos, colóquelos correctamente. Gracias por registrarse con nosotros.'.
                ].
        ]
        ifFalse: [
            "Si el usuario existe, pedir la clave"
            clave := Prompter prompt: 'Ingrese su clave'.

            "Si la clave coincide, salir del bucle"
            (usuario clave = clave)
                ifTrue: [seguir := false].
        ].
].! !

!Supermercado categoriesForMethods!
altaArticulo!public! !
altaDistribuidor!public! !
altaUsuario!public! !
buscarArticulo:!public! !
buscarDistribuidor:!public! !
buscarUsuario:!public! !
inicializar!public! !
listarVenta!public! !
mostrarArticulos!public! !
registrarVenta:!public! !
validarUsuario!public! !
!

Usuario guid: (GUID fromString: '{21f0e7b7-2776-41b3-958c-e38cb41d0353}')!

Usuario comment: ''!

!Usuario categoriesForClass!Ejercicio1! !

!Usuario methodsFor!

cargarDatos

clave:= Prompter prompt:'Ingresar clave'.
nombre := Prompter prompt: 'Ingresar nombre' .
domicilio:= Prompter prompt:'Ingrese domicilio'.
zona := (Prompter prompt:'Ingrese zona') asNumber.
id:= Codigo.
Usuario aumentarCodigo.!

domicilio
    ^domicilio!

id
^id.!

nombre
    ^nombre!

zona
^zona.! !

!Usuario categoriesForMethods!
cargarDatos!public! !
domicilio!public! !
id!public! !
nombre!public! !
zona!public! !
!

!Usuario class methodsFor!

aumentarCodigo
Codigo := Codigo + 1.!

inicializar
Codigo:= 1.! !

!Usuario class categoriesForMethods!
aumentarCodigo!public! !
inicializar!public! !
!

Venta guid: (GUID fromString: '{8402242d-9557-472d-865d-a61cdb90b116}')!

Venta comment: ''!

!Venta categoriesForClass!Ejercicio1! !

!Venta methodsFor!

agregarArticuloCantidad:unArticulo
|item|
"Crear ítem de venta con artículo y cantidad"
item := ArticuloCantidad new.
item articulo: unArticulo.
item cantidad: (Prompter prompt: 'Ingrese la cantidad que desea agregar ') asNumber.
articulosCantidad add: item.!

calcularTotal
|total art can pre|
total := 0.
articulosCantidad do: [:ac | 
    art :=  ac articulo.
    can:= ac cantidad.
    pre := (art obtenerPrecio) * can.
    total := total + pre.
].
^total.!

cargarDatos
codigo := Prompter prompt: 'Ingrese el codigo de la venta' asNumber.
fecha := Date today.
articulosCantidad := OrderedCollection new.!

distribuidor
    ^distribuidor!

distribuidor: unDistribuidor
    distribuidor := unDistribuidor!

fecha
^fecha.!

usuario
    ^usuario
!

usuario: unUsuario
    usuario := unUsuario! !

!Venta categoriesForMethods!
agregarArticuloCantidad:!public! !
calcularTotal!public! !
cargarDatos!public! !
distribuidor!public! !
distribuidor:!public! !
fecha!public! !
usuario!public! !
usuario:!public! !
!

ArticuloComun guid: (GUID fromString: '{6c504864-d306-4772-a6a8-e33c6d33afdf}')!

ArticuloComun comment: ''!

!ArticuloComun categoriesForClass!Ejercicio1! !

ArticuloFrio guid: (GUID fromString: '{89640616-2e78-4ddc-ab03-fa4165a29fec}')!

ArticuloFrio comment: ''!

!ArticuloFrio categoriesForClass!Ejercicio1! !

!ArticuloFrio methodsFor!

cargarDatos
super cargarDatos.
peso:= (Prompter prompt:'Ingrese el peso del articulo') asNumber.!

obtenerPrecio
^precio + (peso * Porcentaje)
! !

!ArticuloFrio categoriesForMethods!
cargarDatos!public! !
obtenerPrecio!public! !
!

!ArticuloFrio class methodsFor!

inicializar
Porcentaje := (Prompter prompt: 'Ingresar porcentaje') asNumber.! !

!ArticuloFrio class categoriesForMethods!
inicializar!public! !
!

"Binary Globals"!

