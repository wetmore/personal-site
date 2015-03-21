//setTimeout(positionSidenotes, 1);

//window.addEventListener("resize", positionSidenotes);

MathJax.Hub.Register.StartupHook("End",function () {
  positionSidenotes();
});

function toArray(nodeList) {
  return Array.prototype.slice.apply(nodeList);
}

function positionSidenotes() {
  var notes = toArray(document.getElementsByClassName('sidenote'));
  notes.forEach(function(sn) {
    var ol = sn.getElementsByTagName('ol')[0];
    var isStart = function(n) { return n == +ol.getAttribute('start') };
    var children = toArray(ol.childNodes);
    
    children.forEach(function(li) {
      var noteNum = +li.id.substr(2);
      var ref = document.getElementById('fnref' + noteNum);
      if (ref && li.offsetParent) {
        var delta = ref.offsetTop - (li.offsetParent.offsetTop + li.offsetTop);

        if (isStart(noteNum)) { // reposition sidenote
          li.style.marginTop = delta + "px";
        } else { // reposition li
          li.style.marginTop = delta + "px";
        }
      }
    });
  });
}