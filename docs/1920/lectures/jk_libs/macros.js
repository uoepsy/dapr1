remark.macros.scale = function (val) {
  var url = this;
  return '<img src="' + url + '" style="width:' + val + ';height:' + val + '" />';
};


/* absolute position of (top left of) image */
/* see https://yongfu.name/2019/04/29/xaringan_tips.html */
remark.macros.abs = function(width="30%", left="85%", top="15%", cl="") {
var url = this;
return '<img src="' + url + '" style="position:absolute;left:' + left + ';top:' + top + ';width:' + width + '" class="' + cl + '" />';
};
