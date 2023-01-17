' 2D Snooker physics (basic simulation)- Jim Brown (2011)
' v1.02 - Added additional comments and relabelled variables
' Credits: Joseph Humfrey
' NOTE: Uses the "Phone" library (keep phone awake)
'
' heavily changes by jepalza (Joseba Epalza <jepalza_at_gmail_com> 2022)
'
' using CSFML audio Library from D.J. Peters:
' https://www.freebasic.net/forum/viewtopic.php?f=14&t=25036&p=224432

'#Include "fbgfx.bi"
#Include "windows.bi" ' only for BOOL

' Initialize BASS
#Include "bass.bi"
BASS_Init(-1, 44100, 0, 0, 0)

' prepare sounds effect (examples from 'freebasic examples' dir)
Dim As String fname

fname = "effect/poolballhit.wav"
Dim As HSAMPLE ballhit = BASS_SampleLoad(0, StrPtr(fname), 0, 0, 16, 0)
Dim Shared poolballhit As HCHANNEL 
poolballhit = BASS_SampleGetChannel(ballhit, 0)

fname = "effect/poolballborder.wav"
Dim As HSAMPLE ballborder = BASS_SampleLoad(0, StrPtr(fname), 0, 0, 16, 0)
Dim Shared poolballborder As HCHANNEL 
poolballborder = BASS_SampleGetChannel(ballborder, 0)

BASS_ChannelPlay(poolballhit, 0)
BASS_ChannelPlay(poolballborder, 0)

 Randomize timer

	' Process_Globals
	Dim Shared FRICTION  As Single		' fiction of the balls
	Dim Shared BALL_MASS  As Single		' weight of the balls
	Dim Shared BALL_RADIUS As Single	' radius of the balls
	Dim Shared BALL_DIAMETER As Single	' diameter of the balls
	Dim Shared NUMBALLS As Integer			' how many balls to control
	Dim Shared ballsmoving As BOOL	' flag to indicate whether or not any of the balls are moving
		
	Dim Shared As Single mouseX,mouseY
	Dim Shared As Single cueAngle,cuePower

	
	Type balltype 
		x As Single
		y As Single
		dx As Single
		dy As Single
		color_ As Integer
	End Type
	Dim Shared ball(50) As balltype


	' Globals
	Dim Shared As Integer centerX,centerY	' center of the snooker table display area (panel 2)
	Dim Shared As Integer powerBarX,powerBarY,powerBarWidth,powerBarHeight	' power bar positions (panel 1)
	Dim Shared As Integer count
	Dim Shared As BOOL win1_Invalidate=FALSE
	
	' display-related variables
	Type Win 
		As Integer WinXpos
		As Integer WinYpos
		As Integer WinHeight
		As Integer WinWidth
	End Type
	Dim Shared As Win win1,win2

	
	Declare Sub RenderPanel1()
	Declare Sub RenderPanel2()
	Declare Sub UpdatePhysics()
	Declare Sub SetupTriangle()
	Declare Sub SetupCueBall()
	Declare Function RNum() As Single
	Declare Function AreBallsMoving() As BOOL
	Declare Function RangeRND(a As Integer, b As Integer) As Single


' user has touched Panel 1 (the left panel)
Sub panel1_Touch (X As Single, Y As Single)
	If ballsmoving=TRUE Or win1_Invalidate=TRUE Then Return
	' check if touched point is over the 'power bar' indicator
	If Y>=powerBarY AND Y<=powerBarHeight Then
		Dim As Integer range=(powerBarHeight-powerBarY)
		Dim As single scale=range/100
		cuePower=(range-(Y-powerBarY-5))/scale
		If cuePower<5 Then cuePower=5
		If cuePower>100 Then cuePower=100
		RenderPanel1()
		Return
	End If
	' check if touch point is over the 'take shot' ball
	If Y>powerBarY+powerBarHeight And Y<powerBarY+powerBarHeight+(BALL_DIAMETER*2) Then
		cueAngle=ATan2(mouseX-ball(0).X,mouseY-ball(0).Y)
		ball(0).dx=Sin(cueAngle)*cuePower
		ball(0).dy=Cos(cueAngle)*cuePower
		Color(RGB(0,0,0))
		win1_Invalidate=TRUE
		ballsmoving=TRUE
	End If
End Sub

' user has touched Panel 2 (the snooker table panel)
Sub panel2_Touch (X As Single, Y As Single)
	mouseX=X : mouseY=Y 
	RenderPanel2()
End Sub


Sub Run_All()
	count=count+1
	
	Cls
	RenderPanel1()
	RenderPanel2()
	screencopy
	
	If ballsmoving=TRUE Then
		UpdatePhysics()
		RenderPanel2()
		If count Mod 20=0 Then
			If AreBallsMoving=FALSE Then
				ballsmoving=FALSE : RenderPanel1() : RenderPanel2()
			End If
		End If
	End If
End Sub

Sub RenderPanel1()

	' power bar indicator
	Dim As Integer col
	Dim As Integer range=(powerBarHeight-powerBarY)
	Dim As Single  scale=range/100
	Dim As Integer bargr=powerBarY+5+(range -(cuePower*scale))
	Line(powerBarX,powerBarY)-(powerBarWidth,powerBarHeight),RGB(0,0,230),bf
	' if we wants ramp effect
	For f As Integer=bargr To powerBarHeight-5
		If ((f/8) Mod 2)=0 Then col=RGB(255,255,255) Else col=RGB(200,200,200)
		Line(powerBarX+(f/scale)-10,f)-(powerBarWidth-5,f),col
	Next
	' little bug !! needs to clear upper-left corner of bar... 
	Line(powerBarX,powerBarY)-step(5,50),RGB(0,0,230),bf
	Line(0,0)-step(10,50),RGB(0,0,0),bf
	' if we wants only rectangle
	'Line(powerBarX+5,powerBarY+5+(range -(cuePower*scale)))-(powerBarWidth-5,powerBarHeight-5),RGB(255,255,255),bf
	' take shot ball
	Circle(powerBarX+(powerBarWidth/2.0),powerBarY+powerBarHeight+50),win1.WinWidth/2-20,RGB(200,200,200),,,,f
	Circle(powerBarX+(powerBarWidth/2.0),powerBarY+powerBarHeight+50),win1.WinWidth/2-26,RGB(255,255,255),,,,f

End Sub

Sub RenderPanel2()
	
	' borde
	Line(win2.WinXpos+5,win2.WinYpos+5)-Step(win2.WinWidth-8,win2.WinHeight-8),RGB(150,50,0),bf 
	' mesa
	Line(win2.WinXpos+20,win2.WinYpos+20)-step(win2.WinWidth-40,win2.WinHeight-40),RGB(16,127,78),bf 
		
	' Draw each ball. Note that ball(0) is the cue ball
	Dim As integer col,col2
	For i As Integer=0 To NUMBALLS
		' complex, but simple system to get shaded balls without images or sprites
		col2=ball(i).color_
		For g As Integer=0 To BALL_RADIUS-2
			If col2=1 Then col=RGB(255-((BALL_RADIUS-g)*3),255-((BALL_RADIUS-g)*3),255-((BALL_RADIUS-g)*3))
			If col2=2 Then col=RGB(255-((BALL_RADIUS-g)*3),0,0)
			If col2=3 Then col=RGB(255-((BALL_RADIUS-g)*3),255-((BALL_RADIUS-g)*3),0)
			Circle(ball(i).x, ball(i).y), BALL_RADIUS-g, col,,,,f
		Next
	Next
	
	' Render the aiming line and circle (only when all balls have stopped moving)
	If ballsmoving=FALSE Then
		win1_Invalidate=FALSE
		Line (ball(0).x,ball(0).y)-(mouseX,mouseY),RGB(0,0,0),,&hF0F0
		Circle (mouseX,mouseY),BALL_RADIUS,RGB(0,0,0)
	End If

End Sub

' Arrange balls in a triangle formation
Sub SetupTriangle()
	Dim As Integer ballTriangleSize,i,xloop,yloop
	i=0
	Do Until i>=NUMBALLS
		ballTriangleSize=ballTriangleSize+1
		i=i+ballTriangleSize
	Loop
	i=1
	Dim As Integer adjustX= win2.WinWidth/3
	Dim As Integer adjustY=-win2.WinHeight/2 -8
	For xloop=ballTriangleSize To 1 Step -1
		For yloop=1 To xloop
			ball(i).x=adjustX+((5+xloop)*(BALL_DIAMETER*1.2)+150+RNum)
			ball(i).y=adjustY+((yloop*(BALL_DIAMETER*1.2))-(xloop*(BALL_DIAMETER*1.2))/2+(centerX)+RNum)
			ball(i).dx=0.0
			ball(i).dy=0.0
			' yellow or red ball colour
			If i Mod(2)=0 Then
				ball(i).color_=2 'RGB(210,30,20)
			Else
				ball(i).color_=3 'RGB(240,200,18)
			End If
			i=i+1
		Next
	Next
End Sub


' Position the cue ball and set the aiming direction to point above the ball
Sub SetupCueBall()
	Dim As Integer adjustX= win2.WinWidth/5
	ball(0).x=((win2.WinWidth/2)-BALL_RADIUS-RangeRND(60,65))-adjustX ' inital position random, like a real human use
	ball(0).y=(centerY+BALL_RADIUS+RNum)-27
	ball(0).dx=0.0
	ball(0).dy=0.0
	ball(0).color_=1
	mouseY=(centerY+BALL_RADIUS+RangeRND(-10,10))
	'mouseX=(win2.WinHeight/2.75)+350
	mouseX=((win2.WinWidth/2)-BALL_RADIUS)+200
	cuePower=RangeRND(70,80)
End Sub

Sub UpdatePhysics()
	Dim As Integer i,b
	Dim As Single actualDist, collisionNormalAngle, moveDist
	Dim As Single nX,nY,a1,a2,optimisedP
	
	For i=0 To NUMBALLS
		
		' MOVEMENT
		' Update ball postion
		ball(i).x=ball(i).x+ball(i).dx
		ball(i).y=ball(i).y+ball(i).dy
		' Slow the ball down via the global friction value
		ball(i).dx=ball(i).dx*FRICTION
		ball(i).dy=ball(i).dy*FRICTION
		' Stop ball completely when below certain speed
		If Abs(ball(i).dx)<0.068 Then ball(i).dx=0.0
		If Abs(ball(i).dy)<0.068 Then ball(i).dy=0.0
		' COLLISION CHECKS
		' Check each other ball (b) against current ball (i)
		For b=i To NUMBALLS
			' No need to check ball against itself
			If b=i Then Continue For
			' Get the distance between the 2 balls being checked
			actualDist=Sqr( ((ball(b).x-ball(i).x) ^ 2) + ((ball(b).y-ball(i).y) ^ 2) )
			' Collided? Check actual distance against ball diameter
			If actualDist<BALL_DIAMETER Then
				BASS_ChannelPlay(poolballhit, 0)
				' Obtain the angle of ball (b) against ball (i)
				collisionNormalAngle=ATan2(ball(b).y-ball(i).y,ball(b).x-ball(i).x)
				' Position exact touch (no intersection)
				moveDist=(BALL_DIAMETER-actualDist)*0.5
				ball(i).x=ball(i).x+moveDist*Cos(collisionNormalAngle+180)
				ball(i).y=ball(i).y+moveDist*Sin(collisionNormalAngle+180)
				ball(b).x=ball(b).x+moveDist*Cos(collisionNormalAngle)
				ball(b).y=ball(b).y+moveDist*Sin(collisionNormalAngle)
				' COLLISION RESPONSE
				' n = vector connecting centres of balls
				'     Find components normalised vector
				nX=Cos(collisionNormalAngle)
				nY=Sin(collisionNormalAngle)
				' Find length of components movement vectors (via dot product)
				a1=ball(i).dx*nX + ball(i).dy*nY
				a2=ball(b).dx*nX + ball(b).dy*nY
				' Optimised = 2*(a1-a2)/(BallMass1+BallMass2)
				optimisedP=(2.0 * (a1-a2) ) / (BALL_MASS*2)
				' Find resultant vectors
				ball(i).dx=ball(i).dx-(optimisedP*BALL_MASS*nX)
				ball(i).dy=ball(i).dy-(optimisedP*BALL_MASS*nY)
				ball(b).dx=ball(b).dx+(optimisedP*BALL_MASS*nX)
				ball(b).dy=ball(b).dy+(optimisedP*BALL_MASS*nY)
			End If
		Next
		
		' Simple bounce off walls check
		' left and right
		If ball(i).x<(win2.WinXpos+20)+BALL_RADIUS Then
			ball(i).x=(win2.WinXpos+20)+BALL_RADIUS : ball(i).dx=ball(i).dx*-0.9
			BASS_ChannelPlay(poolballborder, 0)
		End If
		If ball(i).x>(win2.WinXpos+win2.WinWidth-20)-BALL_RADIUS Then
			ball(i).x=(win2.WinXpos+win2.WinWidth-20)-BALL_RADIUS : ball(i).dx=ball(i).dx*-0.9
			BASS_ChannelPlay(poolballborder, 0)
		End If
		'  up and down
		If ball(i).y<(win2.WinYpos+20)+BALL_RADIUS Then
			ball(i).y=(win2.WinYpos+20)+BALL_RADIUS : ball(i).dy=ball(i).dy*-0.9
			BASS_ChannelPlay(poolballborder, 0)
		End If
		If ball(i).y>(win2.WinHeight-20)-BALL_RADIUS Then
			ball(i).y=(win2.WinHeight-20)-BALL_RADIUS : ball(i).dy=ball(i).dy*-0.9
			BASS_ChannelPlay(poolballborder, 0)
		End If

	Next
End Sub


Function RangeRND(a As Integer, b As Integer) As Single
	Return (Rnd(1)*Abs(b-a))+a
End Function

' Return TRUE if any of the balls are moving
Function AreBallsMoving() As BOOL
	For obj As Integer=0 To NUMBALLS
		If ball(obj).dx<>0.0 OR ball(obj).dy<>0.0 Then Return TRUE 
	Next
	Return FALSE 
End Function

' Return a random float between -0.5 and +0.5
' Used to add a slight re-positioning of the balls
Function RNum() As Single
	Dim f As Single = RangeRND(-100,100)
	Return f/200.0
End Function






 ' ======================================== MAIN ===============================================


	' display / system setup
	Dim As Integer WinW,WinH
	Screenres 1280,600,32,2
	ScreenSet 1,0
	ScreenInfo WinW,WinH

	win1.WinXpos=0
	win1.WinYpos=0
	win1.WinWidth=(WinW/10)
	win1.WinHeight=WinH-1
	
	win2.WinXpos=(WinW/10)+1
	win2.WinYpos=0
	win2.WinWidth=(WinW-win2.WinXpos)-1
	win2.WinHeight=WinH-1
	
	Line(win1.WinXpos,win1.WinYpos)-(win1.WinWidth,win1.WinHeight),RGB(100,100,100),b
	Line(win2.WinXpos,win2.WinYpos)-step(win2.WinWidth,win2.WinHeight),RGB(150,150,150),b

	' table center
	centerX=win2.WinWidth/2
	centerY=win2.WinHeight/2
	
	' How many pottable balls in game (excluding cue ball)
	' NOTE: Use 3,6,10,15,21 ... (since the balls are placed in a triangle format)
	NUMBALLS=15
	
	' General physics settings
	FRICTION=0.981
	BALL_MASS=60.0
	BALL_RADIUS=win2.WinWidth/(NUMBALLS*3.0)
	BALL_DIAMETER=BALL_RADIUS*2.0

	' power bar position and dimensions
	powerBarX=10
	powerBarY=10
	powerBarWidth=win1.WinWidth-10-10
	powerBarHeight=win1.WinHeight-(128*2)

	' put Triangle of balls
	SetupTriangle()
	
	' setup our ball
	' note: inital position and power are random into a little range!!! (like real human use)
	SetupCueBall()
	
	ballsmoving=FALSE ' first time
	RenderPanel1()
	RenderPanel2()
	
   Dim As Integer win1_mouseX,win1_mouseY,win1_mouseB
   Dim As Integer win2_mouseX,win2_mouseY,win2_mouseB
   
	While inkey()<>chr(27)
		
	  if GetMouse(win1_mouseX,win1_mouseY)=0 then
	    If win1_mouseX>0 And win1_mouseY>0 Then 
	      If win1_mouseX<(WinW/10) Then
	        if GetMouse(win1_mouseX,win1_mouseY,,win1_mouseB)=0 then
	          If win1_mouseB And 1 Then panel1_Touch(win1_mouseX,win1_mouseY)
	        endif
	      Else
	        if GetMouse(win2_mouseX,win2_mouseY,,win2_mouseB)=0 then
	          If win2_mouseB And 1 Then panel2_Touch(win2_mouseX,win2_mouseY)
	        endif  
	      EndIf
	    EndIf
	  endif  
	  
	  Run_All()	
	  
	  Sleep 5 ' try this into 1-50 for example
	  
	Wend

 BASS_Free()  
