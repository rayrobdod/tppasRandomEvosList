/**
 * Enable or disable the custom-bst-selectors based on whether the
 * custom-bst-delta-radiobutton is selected.
 */
document.addEventListener("DOMContentLoaded", function() {
	function forEach(coll, fun) {
		for (i = 0; i < coll.length; i++) {
			fun(coll[i]);
		}
	}

	function syncBstEnabled() {
		var checked = document.getElementById("bstdifference_custom").checked;
		document.getElementById("bstdifference_min").disabled = !checked;
		document.getElementById("bstdifference_max").disabled = !checked;
	}

	syncBstEnabled();
	forEach(document.querySelectorAll("input[name=\"bstdifference\"]"), function(input) {
		input.addEventListener("change", syncBstEnabled);
	});
})
