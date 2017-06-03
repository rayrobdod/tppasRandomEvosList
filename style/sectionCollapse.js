document.addEventListener("DOMContentLoaded", function() {
	let headers = document.getElementsByTagName("h2")
	const SHOWN_INDICATOR = "▼ "
	const HIDEN_INDICATOR = "▶ "
	
	function toArray(x) {
		var retVal = new Array()
		for (i = 0; i < x.length; i++) {
			retVal.push(x[i])
		}
		return retVal
	}
	
	function toggleSectionVisible(header) {
		let isShown = header.dataset["shown"] == "true"
		for (var elem = header.nextElementSibling; elem != null && elem.localName != "h2"; elem = elem.nextElementSibling) {
			elem.style.display = (isShown ? "none" : "")
		}
		header.innerText = (isShown ? HIDEN_INDICATOR : SHOWN_INDICATOR) + header.innerText.substring(2)
		header.dataset["shown"] = !isShown
	}
	
	toArray(headers)
		.forEach(function(header) {
			header.dataset["shown"] = true
			header.style.cursor = "pointer"
			header.innerText = SHOWN_INDICATOR + header.innerText
			header.addEventListener("click", function() {
				toggleSectionVisible(header)
			});
		})
})
