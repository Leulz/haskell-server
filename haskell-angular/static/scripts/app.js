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
}])

app.service('Comment', ['$http', function ($http) {
    this.post = function (comment) {
        return $http
        .post('http://localhost:3000/comments', {message: comment})
    }
}])
