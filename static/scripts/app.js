var app = angular.module('app', []);

app.run(['$rootScope', function ($rootScope) {
    $rootScope.title = 'home';
}]);