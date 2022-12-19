;;; org-food.el --- Let org-mode feed you.

;; Copyright (C) 2014 Free Software Foundation, Inc.

;; Author: jhrr@users.noreply.github.com
;; Maintainer: jhrr@users.noreply.github.com
;; Homepage: http://www.github.com/jhrr/org-food
;; Created: 2014.02
;; Version: 0.1
;; Keywords: diet, food, recipes, day-planning, habits, health
;; Package-Requires:

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; I know many good recipes, but when it is time to cook or I am out
;; shopping for food, my memory often fails me. As a result, I end up eating
;; the same few meals over and over again. Or sometimes, when feeling
;; completely uninspired, not even bothering to eat at all. It shouldn't be
;; this way, not with org-mode in our lives.

;; Making a recipe can be seen as a particular type of habit, ie a recurring
;; pattern of behaviour.

;;; Code:

(require 'org)
(eval-when-compile
  ;; TODO: use-package -> cl-defstruct & cl-generic is all we need, at first.
  (require 'cl-lib))

;; Customisation group
(defgroup org-food nil
  "Options for the configuration of org-food."
  :tag "Org Food"
  :group 'org)

(defcustom org-food-recipes-file nil
  "The file where org-food recipes are stored."
  :group 'org-food
  :type 'file)

(defcustom org-food-minimum-repeat-period 14
  "How many days should pass before a recipe can be suggested again.
If there are less recipes in your database than this value, you might end up
with the same suggestion sooner than this condition would usually allow."
  :group 'org-food
  :type 'integer)

;; TODO: Use CL set operations here -> http://clhs.lisp.se/Body/14_abb.htm
(defconst diet-types '("carnivore" ("pescatarian" ("vegetarian" ("vegan"))))
  "A set of common dietary behaviours.")

(defconst substance-types '("heavy" "medium" "light" "snack")
  "A subjective label for how 'substantial' a particular recipe is.")

;; TODO: https://nullprogram.com/blog/2018/02/14/
(cl-defstruct recipe-metadata
  "Struct containing the metadata for a recipe."
  name chefs locale substance heads diet spicy calories last-cooked)

(defun org-food--parse-recipe ()
  "Extracts metadata from a recipe node at POM.
Returns a `recipe' struct of the following elements:

  - The name of the dish.
  - The name of the chef(s) who created the recipe.
  - Where the dish is from.
  - How substantial (filling) the recipe is.
  - How many people the recipe feeds.
  - The kinds of diet the recipe is suitable for.
  - If the recipe is spicy or not.
  - Calories per serving.
  - Preparation time.
  - Last cooked date.
  - Required ingredients.
  - Cooking method.
  - Miscellaneous comments and observations."
  (ignore))

;;;###autoload
(defun org-food-browse-recipes ()
  "Browse the recipe database."
  (interactive)
  (ignore))

;;;###autoload
(defun org-food-wizard ()
  "Select a meal from org-food based on user specifications."
  (interactive)
  (ignore))

;;;###autoload
(defun org-food-feedme ()
  "Get a recipe recommendation from org-food."
  (interactive)
  (ignore))

;;; org-food-tests

(ert-deftest test-org-food--parse-recipe ()
  "Test parsing recipe metadata into a struct."
  (ignore))

(provide 'org-food)
;;; org-food.el ends here
