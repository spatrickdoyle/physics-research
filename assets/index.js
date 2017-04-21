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
	var boxes = document.getElementsByClassName('expid');

    for (var i = 0; i < boxes.length; i++) {
		console.log(boxes[i].checked);
        boxes[i].checked = true;
		console.log(boxes[i].checked);
    }
}



function toggleState(stems, index, nums) {
	//string[] stems: list of class stems to affect
	//int index: index corresponding to the class names to be raised
	//int[] nums: list of total number of each stem in the page

	//Iterate the list of stems and generate arrays of all elements for each one
	var listOfLists = [];
	var sublist;
	for (var i = 0; i < stems.length; i++) {
		/*sublist = [];
		for (var j = 0; j < nums[i]; j++) {
			sublist = sublist.concat(Array.prototype.slice.call(document.getElementsByClassName(stems[i]+j.toString()),0));
		}
		listOfLists.push(sublist);*/
		listOfLists.push(Array.prototype.slice.call(document.getElementsByClassName(stems[i]+index.toString()),0));
	}
	var finalList;
	if (listOfLists.length > 1) {
		finalList = intersection(listOfLists);
	}
	else {
		finalList = listOfLists[0];
	}
	//console.log(finalList[0]);

	//Lower EVERYTHING that isn't finalList[0], if it isn't raised, and raise finalList[0]
	/*var collection;
	for (var i = 0; i < stems.length; i++) {
		for (var j = 0; j < nums[i]; j++) {
			collection = document.getElementsByClassName(stems[i]+j.toString());
			for (var k = 0; k < collection.length; k++) {
				if (collection[k] != finalList[0]) {
					lower(collection[k]);
				}
			}
		}
	}
	raise(finalList[0]);*/

	//Toggle finalList[0]
	finalList[0].classList.toggle("fadeIn");
	finalList[0].classList.toggle("fadeOut");
}

function changeState(stems, index, nums) {
	//string[] stems: list of class stems to affect
	//int[] indices: indices corresponding to the class names to be raised
	//int[] nums: list of total number of each stem in the page

	//Iterate the list of stems and generate arrays of all elements for each one
	var listOfLists = [];
	var sublist;
	for (var i = 0; i < stems.length; i++) {
		/*sublist = [];
		for (var j = 0; j < nums[i]; j++) {
			sublist = sublist.concat(Array.prototype.slice.call(document.getElementsByClassName(stems[i]+j.toString()),0));
		}
		listOfLists.push(sublist);*/
		//console.log(document.getElementsByClassName(stems[i]+index[i].toString()));
		listOfLists.push(Array.prototype.slice.call(document.getElementsByClassName(stems[i]+index[i].toString()),0));
	}
	var finalList;
	if (listOfLists.length > 1) {
		finalList = intersection(listOfLists);
	}
	else {
		finalList = listOfLists[0];
	}

	if (stems[0] == "mode") {
		for (var i = 0; i < 5; i++) {
			if (document.getElementById("figtype-"+i.toString()).checked == true) {
				ind = i;
			}
		}
	}
	else {
		ind = 0;
	}
	console.log(finalList[ind]);

	//Lower EVERYTHING that isn't finalList[ind], if it isn't raised, and raise finalList[0]
	var collection;
	for (var i = 0; i < stems.length; i++) {
		for (var j = 0; j < nums[i]; j++) {
			collection = document.getElementsByClassName(stems[i]+j.toString());
			for (var k = 0; k < collection.length; k++) {
				if (collection[k] != finalList[ind]) {
					lower(collection[k]);
				}
			}
		}
	}
	raise(finalList[ind]);

	if (stems[0] == "highlight") {
		for (var i = 0; i < 3; i++) {
			if (document.getElementById("wtype"+index[0].toString()+"-"+i.toString()).checked == true) {
				changeState(["mode"],[i],[3]);
			}
		}
	}
}

function lower(element) {
	if (element.className.search('fadeIn') != -1) {
		element.classList.toggle("fadeIn");
		element.classList.toggle("fadeOut");
	}
}

function raise(element) {
	if (element.className.search('fadeOut') != -1) {
		element.classList.toggle("fadeIn");
		element.classList.toggle("fadeOut");
	}
}

function intersection(listOfLists) {
	var intersect = []
	//Iterate through each list
	for (var i = 0; i < listOfLists.length; i++) {
		//For each list, iterate through the list...
		for (var j = 0; j < listOfLists[i].length; j++) {
			//...and iterate through every *other* list to check for collisions
			for (var k = i+1; k < listOfLists.length; k++) {
				for (var l = 0; l < listOfLists[k].length; l++) {
					if (listOfLists[i][j] == listOfLists[k][l]) {
						//Check if the collision is already in the output list
						var flag = true;
						for (var m = 0; m < intersect.length; m++) {
							if (intersect[m] == listOfLists[k][l]) {
								flag = false;
								break;
							}
						}
						if (flag) {
							intersect.push(listOfLists[k][l]);
						}
					}
				}
			}
		}
	}
	return intersect;
}


window.onload = function () {
	document.getElementById("resetbutton").onclick = function () {
		window.location.href = window.location.href.split("?")[0];
	};
};
