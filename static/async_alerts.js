var Notifier=new function(){
	// real alert function placeholder
	this._alert=null;
	// return Notifier object methods
	return {
		// m=message,c=classname
		notify:
			function(m,c){
				// we may consider adding frames support
				var w=this.main;
				// shortcut to document
				var d=this.main.document;
				// canvas, window width and window height
				var r=d.documentElement;
				var ww=w.innerWidth?w.innerWidth+w.pageXOffset:r.clientWidth+r.scrollLeft;
				var wh=w.innerHeight?w.innerHeight+w.pageYOffset:r.clientHeight+r.scrollTop;
				// create a block element
				var b=d.createElement('div');
				b.id='Message';
				b.className=c||'';
				b.style.cssText='top:-9999px;left:-9999px;position:absolute;white-space:nowrap;';
				// classname not passed, set defaults
				if(b.className.length==0){
					b.style.margin='0px 0px';
					b.style.padding='8px 8px';
					b.style.border='1px solid #f00';
					b.style.backgroundColor='#fc0';
				}
				// insert block in to body
				b=d.body.insertBefore(b,d.body.firstChild);
				// write HTML fragment to it
				b.innerHTML=m;
				// save width/height before hiding
				var bw=b.offsetWidth;
				var bh=b.offsetHeight;
				// hide, move and then show
				b.style.display='none';
				b.style.top=Math.random()*(wh-bh)+'px';// random y position
//				b.style.top=wh-bh+'px';// this is to place it to the bottom
				b.style.left=Math.random()*(ww-bw)+'px';// random x position
//				b.style.left=ww-bw+'px';// this is to place it to the right
				b.style.display='block';
				// fadeout block if supported
				setFading(b,100,0,2000,function(){d.body.removeChild(b);});
			},
		// initialize Notifier object
		init:
			function(w,s){
				// save window
				this.main=w;
				this.classname=s||'';
				// if not set yet
				if(this._alert==null){
					// save old alert function
					this._alert=this.main.alert;
					// redefine alert function
					this.main.alert=function(m){
						Notifier.notify(m,s)
					}
				}
			},
		// shutdown Notifier object
		shut:
			function(){
				// if redifine set
				if(this._alert!=null){
					// restore old alert function
					this.main.alert=this._alert;
					// unset placeholder
					this._alert=null;
				}
			}
	};
};

// apply a fading effect to an object
// by applying changes to its style
// @o = object style
// @b = begin opacity
// @e = end opacity
// @d = duration (millisec)
// @f = function (optional)
function setFading(o,b,e,d,f){
	var t=setInterval(
		function(){
			b=stepFX(b,e,2);
			setOpacity(o,b/100);
			if(b==e){
				if(t){clearInterval(t);t=null;}
				if(typeof f=='function'){f();}
			}
		},d/50
	);
}

// set opacity for element
// @e element
// @o opacity
function setOpacity(e,o){
	// for IE
	e.style.filter='alpha(opacity='+o*100+')';
	// for others
	e.style.opacity=o;
}

// increment/decrement value in steps
// checking for begin and end limits
//@b begin
//@e end
//@s step
function stepFX(b,e,s){
	return b>e?b-s>e?b-s:e:b<e?b+s<e?b+s:e:b;
}

var __alert=window.alert;

Notifier.init(window, 'notifier');
