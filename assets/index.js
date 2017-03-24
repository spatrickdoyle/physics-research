function UrlExists(url) {
    var http = new XMLHttpRequest();
    http.open('HEAD',url,false);
    http.send();
    return http.status != 404;
}

function toggleBlock2() {
	block2 = document.getElementById("level2");
	block2.classList.toggle("fadeIn");
	block2.classList.toggle("fadeOut");
}

function checkAllExps() {
	console.log("running");
	var boxes = document.getElementsByClassName('expid');
	console.log(boxes)

    for (var i = 0; i < boxes.length; i++) {
		console.log(boxes[i].checked)
        boxes[i].checked = isChecked;
    }
}

window.onload = function () {
	document.getElementById("resetbutton").onclick = function () {
		window.location.href = window.location.href.split("?")[0];
	};
};
