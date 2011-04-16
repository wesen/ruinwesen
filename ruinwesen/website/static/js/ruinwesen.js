/*
 * Ruinwesen.com javascript application
 *
 * (c) Apr 2011 - Manuel Odendahl - wesen@ruinwesen.com
 */

/***************************************************************************
 *
 * Main application
 *
 ***************************************************************************/

var App = {
  Views: {},
  Collections: {},
  Controllers: {},
  init: function() {
    new App.Controllers.Orders();
    Backbone.history.start();
    }
};

/***************************************************************************
 *
 * Models
 *
 ***************************************************************************/

var Order = Backbone.Model.extend ({
  url: function() {
    var base = '/backbone/orders';
    if (this.isNew()) return base;
    return base + (base.charAt(base.length - 1) == '/' ? '' : '/') + this.id;
  }
});

/** The orders collection */
App.Collections.Orders = Backbone.Collection.extend({
  model: Order,
  url: "/backbone/orders"
});

/***************************************************************************
 *
 * Controllers
 *
 ***************************************************************************/

App.Controllers.Orders = Backbone.Controller.extend({
  routes: {
    "preorders/:id": "edit",
    "":              "index",
    "newOrder":           "newOrder",
    "delete-preorder/:id": "deleteOrder"
  },

  edit: function (id) {
    var order = new Order({id: id});
    order.fetch({
      success: function(model, resp) {
        new App.Views.OrderEdit({ model: order});
      },
      error: function() {
        new Error({message: "Could not find that preorder."});
        window.location.hash = "#";

      }
    });
  },

  index: function () {
    var orders = new App.Collections.Orders();
    this.orders = orders;
    orders.fetch({
      success: function () {
        new App.Views.OrderList({collection: orders});
        },
      error: function() {
        new Error({message: "Error loading orders."});
      }
    });
  },

  newOrder: function() {
    new App.Views.OrderEdit({model: new Order() });
  },

  deleteOrder: function (id) {
    var order = this.orders.get(id);
    console.log(order);
    order.destroy();
    window.location.hash = "#";
    new App.Views.Notice({message: "Deleted!"});
  }

});


/***************************************************************************
 *
 * Views
 *
 ***************************************************************************/

App.Views.Notice = Backbone.View.extend({
  className: "success",
  displayLength: 5000,
  defaultMessage: '',

  initialize: function() {
    _.bindAll(this, 'render');
    this.message = this.options.message || this.defaultMessage;
    this.render();
  },

  render: function() {
    var view = this;

    $(this.el).html(this.message);
    $(this.el).hide();
    $('#notice').html(this.el);
    $(this.el).slideDown();
    $.doTimeout(this.displayLength, function () {
      $(view.el).slideUp();
      $.doTimeout(2000, function () {
        view.remove();
      });
    });

    return this;
  }
});

App.Views.Error = App.Views.Notice.extend({
  className: "error",
  defaultMessage: 'Uh oh! Something went wrong. Please try again.'
});

/** Show all the orders as a list. */
App.Views.OrderList = Backbone.View.extend({
  initialize: function() {
    this.render();
  },

  render: function() {
    $(this.el).html(ich.order_tpl({orders: this.collection.toJSON()}));
    $('#app').html(this.el);
  }
});

/** Edit an order. */
App.Views.OrderEdit = Backbone.View.extend({
  events: {
    "submit": "save"
  },

  initialize: function() {
    _.bindAll(this, 'render');
    this.model.bind('change', this.render);
    console.log("init order edit");
    this.render();
  },

  save: function() {
    var self = this;
    var msg = this.model.isNew() ? "Successfully created!" : "Saved!";

    this.model.save({ name: this.$("[name=name]").val(),
                      email: this.$("[name=email]").val()},
                    {
                      success: function (model, resp) {
                        new App.Views.Notice({message: msg});
                        Backbone.history.saveLocation('preorders/' + model.id);
                      },
                      error: function () {
                        new App.Views.Error();
                      }
                    });

    return false;
  },

  render: function() {
    console.log(this.model.toJSON());
    $(this.el).html(ich.order_edit_tpl(this.model.toJSON()));
    $('#app').html(this.el);

    this.delegateEvents();
  }
});