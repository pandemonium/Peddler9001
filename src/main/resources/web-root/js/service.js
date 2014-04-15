function getOrderById(id) {
    $.ajax({
    	url: "http://localhost:8080/orders/2",
    	type: "GET",
    	timeout: 30000,
    	headers: {
    		"Content-Type": "application/json"
    	},
    	success:function (data, textStatus) {
    		console.log("Received response HTTP "+textStatus+" (http://localhost:8080/orders/2)");
    		console.log(data);
    	},
    	error: function (jqXHR, textStatus, errorThrown) {
    		console.log("Error during request "+textStatus+" (http://localhost:8080/orders/2)");
    		console.log(errorThrown);
    	},
    });
}