/**
 * Allow a user to sort tables by clicking on the corresponding header.
 * The tables are sorted based on the value data-sort values in that column.
 */
// Global so that the function can be recalled when new tables are created by the theoretical page's scripts
function makeTablesSortable() {
	let tables = document.getElementsByTagName("table");
	const SORT_INDICATOR = " ↕";
	
	function forEach(coll, fun) {
		for (i = 0; i < coll.length; i++) {
			fun(coll[i])
		}
	}
	function toArray(x) {
		var retVal = new Array()
		for (i = 0; i < x.length; i++) {
			retVal.push(x[i])
		}
		return retVal
	}
	
	function sortTable(tableToSort, columnNo) {
		let initial = toArray(tableToSort.tBodies[0].rows);
		function compareRows(a,b) {
			if (a.cells[columnNo].dataset["sort"] < b.cells[columnNo].dataset["sort"]) { return -1; }
			if (a.cells[columnNo].dataset["sort"] > b.cells[columnNo].dataset["sort"]) { return 1; }
			else { return initial.indexOf(a) - initial.indexOf(b); }
		}
		let sortedArray = toArray(initial).sort(compareRows)
		
		var newTableBody = document.createElement("tbody");
		for(var i = 0; i < sortedArray.length; i++) {
			newTableBody.appendChild(sortedArray[i].cloneNode(true));
		}
		tableToSort.replaceChild(newTableBody, tableToSort.tBodies[0]);
	}
	
	function makeColumnSortable(tableToSort, columnNo) {
		let headerCell = tableToSort.tHead.rows[0].cells[columnNo]
		headerCell.style.cursor = "pointer"
		headerCell.appendChild(document.createTextNode(SORT_INDICATOR));
		headerCell.addEventListener("click", function() {
			sortTable(tableToSort, columnNo)
		});
	}
	
	forEach(tables, function(tab) {
		if (tab.tHead != null) {
			var headerCells = tab.tHead.rows[0].cells
			for (j = 0; j < headerCells.length; j++) {
				makeColumnSortable(tab, j)
			}
		}
	})

}

document.addEventListener("DOMContentLoaded", makeTablesSortable)
