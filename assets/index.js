function UrlExists(url) {
    var http = new XMLHttpRequest();
    http.open('HEAD',url,false);
    http.send();
    return http.status != 404;
}

function toggleBlock2(level2) {
	block2 = document.getElementsByClassName(level2);
	for (i = 0; i < block2.length; i++) {
		block2[i].classList.toggle("fadeIn");
		block2[i].classList.toggle("fadeOut");
	}
}

function toggleRadio(level2,list2) {
	//level 2 is an int, the index of the menu to make active
	//list2 is a list of ids of the different menus that are toggled
	for (j = 0; j < list2.length; j++) {
		block2 = document.getElementsByClassName(list2[j]);
		for (i = 0; i < block2.length; i++) {
			if (j != level2) {
				if (block2[i].className.search('fadeIn') != -1) {
					block2[i].classList.toggle("fadeIn");
					block2[i].classList.toggle("fadeOut");
				}
			}
			else {
				if (block2[i].className.search('fadeOut') != -1) {
					block2[i].classList.toggle("fadeIn");
					block2[i].classList.toggle("fadeOut");
				}
			}
		}
	}
}

function changeFunc(selectBox) {
    var selectBox = document.getElementById(selectBox);
    var selectedValue = selectBox.options[selectBox.selectedIndex].value;
    alert(selectedValue);
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
