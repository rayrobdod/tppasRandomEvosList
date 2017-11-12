/**
 * Allow a user to hide or show any sibling nodes following a h2, but not after a
 * different h2, by clicking on that h2
 */
document.addEventListener("DOMContentLoaded", function() {
	let headers = document.getElementsByTagName("h2")
	const SHOWN_INDICATOR = "▼ "
	const HIDEN_INDICATOR = "▶ "
	
	function forEach(coll, fun) {
		for (i = 0; i < coll.length; i++) {
			fun(coll[i])
		}
	}
	
	function toggleSectionVisible(header) {
		let isShown = header.dataset["shown"] == "true"
		for (var elem = header.nextElementSibling; elem != null && elem.localName != "h2"; elem = elem.nextElementSibling) {
			elem.style.display = (isShown ? "none" : "")
		}
		header.innerText = (isShown ? HIDEN_INDICATOR : SHOWN_INDICATOR) + header.innerText.substring(2)
		header.dataset["shown"] = !isShown
	}
	
	forEach(headers, function(header) {
		header.dataset["shown"] = true
		header.style.cursor = "pointer"
		header.innerText = SHOWN_INDICATOR + header.innerText
		header.addEventListener("click", function() {
			toggleSectionVisible(header)
		});
	})
})
