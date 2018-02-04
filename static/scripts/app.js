var app = angular.module('app', ['ngRoute']);

app.run(['$rootScope', function ($rootScope) {
    $rootScope.title = 'home';
}]);

app.config(['$routeProvider', function ($routeProvider) {
    $routeProvider
    .when('/', {
        templateUrl : 'static/templates/layout.html',
        controller  : 'HomeCtrl'
    });
}])

app.controller('HomeCtrl', ['$scope', 'Comment', function ($scope, Comment) {
    $scope.comments = [];
    $scope.post = function () {
        Comment
        .post($scope.message)
        .success(function (data) {
            $scope.comments.push(data);
        })
        .error(function (error) {
            console.log(error);
        });
    };

    var gapiPromise = (function(){
      var deferred = $.Deferred();
      window.onLoadCallback = function(){
        deferred.resolve(gapi);
      };
      return deferred.promise()
    }());

    // This flag we use to show or hide the button in our HTML.
    $scope.signedIn = false; 
    // Here we do the authentication processing and error handling.
    // Note that authResult is a JSON object.
    $scope.processAuth = function(authResult) {
        // Do a check if authentication has been successful.
        if(authResult['access_token']) {
            // Successful sign in.
            $scope.signedIn = true; 
            //     ...
            // Do some work [1].
            //     ...
        } else if(authResult['error']) {
            // Error while signing in.
            $scope.signedIn = false; 
            // Report error.
        }
    }; 
    // When callback is received, we need to process authentication.
    $scope.signInCallback = function(authResult) {
        $scope.$apply(function() {
            $scope.processAuth(authResult);
        });
    }; 
    // Render the sign in button.
    $scope.renderSignInButton = function() {
        gapiPromise.then(() => {
            gapi.signin.render('signInButton',
            {
                'callback': $scope.signInCallback, // Function handling the callback.
                'clientid': '582437371847-trskhnmgsiqmrfrdburledn8juo1esst.apps.googleusercontent.com', // CLIENT_ID from developer console which has been explained earlier.
                'requestvisibleactions': 'http://schemas.google.com/AddActivity', // Visible actions, scope and cookie policy wont be described now,
                // as their explanation is available in Google+ API Documentation.
                'scope': 'https://www.googleapis.com/auth/plus.login https://www.googleapis.com/auth/userinfo.email',
                'cookiepolicy': 'single_host_origin'
            });
        });
    }
    // Start function in this example only renders the sign in button.
    $scope.start = function() {
        $scope.renderSignInButton();
    }; 
    // Call start function on load.
    $scope.start();
}])

app.service('Comment', ['$http', function ($http) {
    this.post = function (comment) {
        return $http
        .post('http://localhost:3000/comments', {message: comment})
    }
}])
