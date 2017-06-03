document.addEventListener("DOMContentLoaded", function() {
	let tables = document.getElementsByTagName("table");
	const SORT_INDICATOR = " ↕";
	
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

	toArray(tables)
		.filter(function(tab) {return tab.classList.contains("pokemon-list")})
		.forEach(function(tab) {
		var headerCells = tab.tHead.rows[0].cells
		for (j = 0; j < headerCells.length; j++) {
			if (j != 2 && j != 3) {
				function doThing(cellIndex) {
					let c = headerCells[cellIndex];
					c.style.cursor = "pointer"
					c.appendChild(document.createTextNode(SORT_INDICATOR));
					c.addEventListener("click", function() {
						sortTable(tab, cellIndex)
					});
				}
				doThing(j)
			}
		}
	})

	toArray(tables)
		.filter(function(tab) {return tab.classList.contains("evolution-list")})
		.forEach(function(tab) {
			var headerCells = tab.tHead.rows[0].cells
			for (j = 0; j < headerCells.length; j++) {
					function doThing(cellIndex) {
						let c = headerCells[cellIndex];
						c.style.cursor = "pointer"
						c.appendChild(document.createTextNode(SORT_INDICATOR));
						c.addEventListener("click", function() {
							sortTable(tab, cellIndex)
						});
					}
					doThing(j)
			}
		})
})
