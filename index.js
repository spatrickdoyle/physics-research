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
