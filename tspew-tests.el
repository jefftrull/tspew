;;; ERT unit tests for tspew -*- mode: Emacs-lisp; lexical-binding: t ; -*-

(require 'tspew)

(defun tspew-test-run-in-buffer (f s)
  "Run test function F in buffer containing only string S"
  (with-temp-buffer
    ;; obey text properties
    (setq-local parse-sexp-lookup-properties t)
    (insert s)
    (let ((end (point)))
      (goto-char 0)
      ;; set up text properties
      (tspew--mark-special-case-symbols (point) end)
      (tspew--mark-special-case-punctuation (point) end))
    (with-syntax-table tspew-syntax-table
      (and (funcall f) (= (point) (point-max))))))


(defun tspew-test-format-in-buffer (f s)
  "Run test function F in buffer containing only string S"
  (with-temp-buffer
    (setq-local tspew--fill-width 20)
    ;; obey text properties
    (setq-local parse-sexp-lookup-properties t)
    (insert s)
    (let ((end (point)))
      (goto-char 0)
      ;; set up text properties
      (tspew--mark-special-case-symbols (point) end)
      (tspew--mark-special-case-punctuation (point) end)
      (with-syntax-table tspew-syntax-table
        (funcall f (point) end)))))

;; these are basically just regression tests, for now

(ert-deftest tspew-parse-tc1 ()
  "Tests extracted from tspew_testcase.cpp"
  ;; functions
  (should (tspew-test-run-in-buffer #'tspew--parse-function "void std::sort(_RAIter, _RAIter) [with _RAIter = _Fwd_list_iterator<vector<Foo<int, char> > >]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "template<class _IteratorL, class _IteratorR> constexpr decltype ((__y.base() - __x.base())) std::operator-(const reverse_iterator<_Iterator>&, const reverse_iterator<_IteratorR>&)"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "template<class _IteratorL, class _IteratorR> constexpr decltype ((__x.base() - __y.base())) std::operator-(const move_iterator<_IteratorL>&, const move_iterator<_IteratorR>&)"))
  ;; types
  (should (tspew-test-run-in-buffer #'tspew--parse-type "std::_Fwd_list_iterator<std::vector<Foo<int, char> > >"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "const std::reverse_iterator<_Iterator>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "const std::move_iterator<_IteratorL>"))
  ;; things we should NOT parse
  (should-not (tspew-test-run-in-buffer #'tspew--parse-type "operator-"))
)

(ert-deftest tspew-parse-tc2 ()
  "Tests extracted from tspew_testcase2.cpp"
  ;; functions
  (should (tspew-test-run-in-buffer #'tspew--parse-function "static void boost::spirit::traits::assign_to_attribute_from_value<Attribute, T, Enable>::call(const T&, Attribute&) [with Attribute = Point3D; T = double; Enable = void]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "void boost::spirit::traits::detail::assign_to(const T&, Attribute&, mpl_::false_) [with T = double; Attribute = Point3D; mpl_::false_ = mpl_::bool_<false>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "void boost::spirit::traits::assign_to(const T&, Attribute&) [with T = double; Attribute = Point3D]"))
  ;; currently failing functions:
  (should (tspew-test-run-in-buffer #'tspew--parse-function "bool boost::spirit::qi::any_real_parser<T, RealPolicies>::parse(Iterator&, const Iterator&, Context&, const Skipper&, Attribute&) const [with Iterator = __gnu_cxx::__normal_iterator<char*, std::__cxx11::basic_string<char> >; Context = boost::spirit::context<boost::fusion::cons<Point3D&, boost::fusion::nil_>, boost::spirit::locals<> >; Skipper = boost::spirit::qi::char_class<boost::spirit::tag::char_code<boost::spirit::tag::space, boost::spirit::char_encoding::ascii> >; Attribute = Point3D; T = double; RealPolicies = boost::spirit::qi::real_policies<double>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "bool boost::spirit::qi::detail::fail_function<Iterator, Context, Skipper>::operator()(const Component&, Attribute&) const [with Component = boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >; Attribute = Point3D; Iterator = __gnu_cxx::__normal_iterator<char*, std::__cxx11::basic_string<char> >; Context = boost::spirit::context<boost::fusion::cons<Point3D&, boost::fusion::nil_>, boost::spirit::locals<> >; Skipper = boost::spirit::qi::char_class<boost::spirit::tag::char_code<boost::spirit::tag::space, boost::spirit::char_encoding::ascii> >]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "bool boost::spirit::qi::sequence_base<Derived, Elements>::parse_impl(Iterator&, const Iterator&, Context&, const Skipper&, Attribute&, mpl_::false_) const [with Iterator = __gnu_cxx::__normal_iterator<char*, std::__cxx11::basic_string<char> >; Context = boost::spirit::context<boost::fusion::cons<Point3D&, boost::fusion::nil_>, boost::spirit::locals<> >; Skipper = boost::spirit::qi::char_class<boost::spirit::tag::char_code<boost::spirit::tag::space, boost::spirit::char_encoding::ascii> >; Attribute = Point3D; Derived = boost::spirit::qi::sequence<boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::nil_> > > >; Elements = boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::nil_> > >; mpl_::false_ = mpl_::bool_<false>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "bool boost::spirit::qi::sequence_base<Derived, Elements>::parse(Iterator&, const Iterator&, Context&, const Skipper&, Attribute&) const [with Iterator = __gnu_cxx::__normal_iterator<char*, std::__cxx11::basic_string<char> >; Context = boost::spirit::context<boost::fusion::cons<Point3D&, boost::fusion::nil_>, boost::spirit::locals<> >; Skipper = boost::spirit::qi::char_class<boost::spirit::tag::char_code<boost::spirit::tag::space, boost::spirit::char_encoding::ascii> >; Attribute = Point3D; Derived = boost::spirit::qi::sequence<boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::nil_> > > >; Elements = boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::cons<boost::spirit::qi::any_real_parser<double, boost::spirit::qi::real_policies<double> >, boost::fusion::nil_> > >]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "bool boost::spirit::qi::phrase_parse(Iterator&, Iterator, const Expr&, const Skipper&, skip_flag, Attr&) [with Iterator = __gnu_cxx::__normal_iterator<char*, std::__cxx11::basic_string<char> >; Expr = boost::proto::exprns_::expr<boost::proto::tagns_::tag::shift_right, boost::proto::argsns_::list2<const boost::proto::exprns_::expr<boost::proto::tagns_::tag::shift_right, boost::proto::argsns_::list2<const boost::spirit::terminal<boost::spirit::tag::double_>&, const boost::spirit::terminal<boost::spirit::tag::double_>&>, 2>&, const boost::spirit::terminal<boost::spirit::tag::double_>&>, 2>; Skipper = boost::proto::exprns_::expr<boost::proto::tagns_::tag::terminal, boost::proto::argsns_::term<boost::spirit::tag::char_code<boost::spirit::tag::space, boost::spirit::char_encoding::ascii> >, 0>; Attr = Point3D]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "bool boost::spirit::qi::phrase_parse(Iterator&, Iterator, const Expr&, const Skipper&, Attr&) [with Iterator = __gnu_cxx::__normal_iterator<char*, std::__cxx11::basic_string<char> >; Expr = boost::proto::exprns_::expr<boost::proto::tagns_::tag::shift_right, boost::proto::argsns_::list2<const boost::proto::exprns_::expr<boost::proto::tagns_::tag::shift_right, boost::proto::argsns_::list2<const boost::spirit::terminal<boost::spirit::tag::double_>&, const boost::spirit::terminal<boost::spirit::tag::double_>&>, 2>&, const boost::spirit::terminal<boost::spirit::tag::double_>&>, 2>; Skipper = boost::proto::exprns_::expr<boost::proto::tagns_::tag::terminal, boost::proto::argsns_::term<boost::spirit::tag::char_code<boost::spirit::tag::space, boost::spirit::char_encoding::ascii> >, 0>; Attr = Point3D]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "bool client::parse_numbers(Iterator, Iterator, Point3D&) [with Iterator = __gnu_cxx::__normal_iterator<char*, std::__cxx11::basic_string<char> >]"))
  )

(ert-deftest tspew-parse-cvc5 ()
  "Tests extracted from a CVC5 error"
  ;; functions
  (should (tspew-test-run-in-buffer #'tspew--parse-function "typename AttrKind::value_type cvc5::internal::expr::attr::AttributeManager::getAttribute(cvc5::internal::expr::NodeValue*, const AttrKind&) const [with AttrKind = cvc5::internal::expr::Attribute<cvc5::internal::theory::SygusVarToTermAttributeId, cvc5::internal::NodeTemplate<true> >; typename AttrKind::value_type = cvc5::internal::NodeTemplate<true>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "typename AttrKind::value_type cvc5::internal::NodeManager::getAttribute(cvc5::internal::TNode, const AttrKind&) const [with AttrKind = cvc5::internal::expr::Attribute<cvc5::internal::theory::SygusVarToTermAttributeId, cvc5::internal::NodeTemplate<true> >; typename AttrKind::value_type = cvc5::internal::NodeTemplate<true>; cvc5::internal::TNode = cvc5::internal::NodeTemplate<false>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "typename AttrKind::value_type cvc5::internal::NodeTemplate<ref_count>::getAttribute(const AttrKind&) const [with AttrKind = cvc5::internal::expr::Attribute<cvc5::internal::theory::SygusVarToTermAttributeId, cvc5::internal::NodeTemplate<true> >; bool ref_count = true; typename AttrKind::value_type = cvc5::internal::NodeTemplate<true>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "typename AttrKind::value_type cvc5::internal::NodeManager::getAttribute(cvc5::internal::TNode, const AttrKind&) const [with AttrKind = cvc5::internal::expr::Attribute<cvc5::internal::theory::arith::ArithPolyNormTag, cvc5::internal::NodeTemplate<true> >; typename AttrKind::value_type = cvc5::internal::NodeTemplate<true>; cvc5::internal::TNode = cvc5::internal::NodeTemplate<false>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "typename AttrKind::value_type cvc5::internal::NodeTemplate<ref_count>::getAttribute(const AttrKind&) const [with AttrKind = cvc5::internal::expr::Attribute<cvc5::internal::theory::arith::ArithPolyNormTag, cvc5::internal::NodeTemplate<true> >; bool ref_count = true; typename AttrKind::value_type = cvc5::internal::NodeTemplate<true>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cvc5::internal::expr::attr::AttrHash<V>::iterator cvc5::internal::expr::attr::AttrHash<V>::find(int) const [with V = cvc5::internal::NodeTemplate<true>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "typename AttrKind::value_type cvc5::internal::expr::attr::AttributeManager::getAttribute(cvc5::internal::expr::NodeValue*, const AttrKind&) const [with AttrKind = cvc5::internal::expr::Attribute<cvc5::internal::theory::arith::RealAlgebraicNumberVarAttributeId, cvc5::internal::NodeTemplate<true> >; typename AttrKind::value_type = cvc5::internal::NodeTemplate<true>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cvc5::internal::Node cvc5::internal::BoundVarManager::mkBoundVar(cvc5::internal::Node, cvc5::internal::TypeNode) [with T = cvc5::internal::expr::Attribute<cvc5::internal::theory::arith::RealAlgebraicNumberVarAttributeId, cvc5::internal::NodeTemplate<true> >; cvc5::internal::Node = cvc5::internal::NodeTemplate<true>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cvc5::internal::Node::substitute<std::__detail::_Node_const_iterator<std::pair<const cvc5::internal::Node&, const cvc5::internal::Node&>, false, true> >(std::__detail::_Node_const_iterator<std::pair<const cvc5::internal::Node&, const cvc5::internal::Node&>, false, true>, std::__detail::_Node_const_iterator<std::pair<const cvc5::internal::Node&, const cvc5::internal::Node&>, false, true>, std::unordered_map<const cvc5::internal::Node&, const cvc5::internal::Node&>&) const::<lambda(const auto:27&)>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "template<class _U1, class _U2, typename std::enable_if<(_MoveConstructiblePair<_U1, _U2>() && _ImplicitlyMoveConvertiblePair<_U1, _U2>()), bool>::type <anonymous> > constexpr std::pair<_T1, _T2>::pair(_U1&&, _U2&&) [with _U2 = _U1; typename std::enable_if<(std::_PCC<true, _T1, _T2>::_MoveConstructiblePair<_U1, _U2>() && std::_PCC<true, _T1, _T2>::_ImplicitlyMoveConvertiblePair<_U1, _U2>()), bool>::type <anonymous> = _U2; _T1 = const cvc5::internal::Node&; _T2 = cvc5::internal::prop::SatValue]"))

  (should (tspew-test-format-in-buffer #'tspew--format-function-region "template<class _U1, class _U2, typename std::enable_if<(_MoveConstructiblePair<_U1, _U2>() && _ImplicitlyMoveConvertiblePair<_U1, _U2>()), bool>::type <anonymous> > constexpr std::pair<_T1, _T2>::pair(_U1&&, _U2&&) [with _U2 = _U1; typename std::enable_if<(std::_PCC<true, _T1, _T2>::_MoveConstructiblePair<_U1, _U2>() && std::_PCC<true, _T1, _T2>::_ImplicitlyMoveConvertiblePair<_U1, _U2>()), bool>::type <anonymous> = _U2; _T1 = const cvc5::internal::Node&; _T2 = cvc5::internal::prop::SatValue]"))


  ;; types
  (should (tspew-test-run-in-buffer #'tspew--parse-type "int"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "uint64_t"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "std::pair<long unsigned int, cvc5::internal::expr::NodeValue*>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "std::__detail::_Insert_base<cvc5::internal::expr::NodeValue*, std::pair<cvc5::internal::expr::NodeValue* const, std::unordered_map<long unsigned int, cvc5::internal::NodeTemplate<true>, std::hash<long unsigned int>, std::equal_to<long unsigned int>, std::allocator<std::pair<const long unsigned int, cvc5::internal::NodeTemplate<true> > > > >, std::allocator<std::pair<cvc5::internal::expr::NodeValue* const, std::unordered_map<long unsigned int, cvc5::internal::NodeTemplate<true>, std::hash<long unsigned int>, std::equal_to<long unsigned int>, std::allocator<std::pair<const long unsigned int, cvc5::internal::NodeTemplate<true> > > > > >, std::__detail::_Select1st, std::equal_to<cvc5::internal::expr::NodeValue*>, std::hash<cvc5::internal::expr::NodeValue*>, std::__detail::_Mod_range_hashing, std::__detail::_Default_ranged_hash, std::__detail::_Prime_rehash_policy, std::__detail::_Hashtable_traits<false, false, true> >::const_iterator"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "std::unordered_map<cvc5::internal::expr::NodeValue*, std::unordered_map<long unsigned int, cvc5::internal::NodeTemplate<true>, std::hash<long unsigned int>, std::equal_to<long unsigned int>, std::allocator<std::pair<const long unsigned int, cvc5::internal::NodeTemplate<true> > > >, std::hash<cvc5::internal::expr::NodeValue*>, std::equal_to<cvc5::internal::expr::NodeValue*>, std::allocator<std::pair<cvc5::internal::expr::NodeValue* const, std::unordered_map<long unsigned int, cvc5::internal::NodeTemplate<true>, std::hash<long unsigned int>, std::equal_to<long unsigned int>, std::allocator<std::pair<const long unsigned int, cvc5::internal::NodeTemplate<true> > > > > > >::iterator"))
  )

(ert-deftest tspew-parse-cib ()
  "Tests extracted from an Intel compile-time-init-build error"
  ;; functions
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::chunk_by<>(const tuple<int, int, int, double, double, bool>&)::<lambda(std::index_sequence<Is ...>)> [with long unsigned int ...Is = {0, 1, 2}; std::index_sequence<Is ...> = std::integer_sequence<long unsigned int, 0, 1, 2>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "constexpr auto cib::chunk_by(Tuple&&) [with Proj = std::type_identity_t; Tuple = const tuple<int, int, int, double, double, bool>&]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::chunk_by<get_service, tuple<detail::extend<TestCallback<0> >, detail::extend<TestCallback<0>, SimpleConfig::<lambda()> > > >(tuple<detail::extend<TestCallback<0> >, detail::extend<TestCallback<0>, SimpleConfig::<lambda()> > >&&)::<lambda(std::index_sequence<Is ...>)> [with long unsigned int ...Is = {0}; std::index_sequence<Is ...> = std::integer_sequence<long unsigned int, 0>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "constexpr auto cib::chunk_by(Tuple&&) [with Proj = get_service; Tuple = tuple<detail::extend<TestCallback<0> >, detail::extend<TestCallback<0>, SimpleConfig::<lambda()> > >]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::chunk_by<get_service, tuple<detail::extend<{anonymous}::TestFlowAlpha>, detail::extend<{anonymous}::TestFlowAlpha, flow::milestone_base> > >(tuple<detail::extend<{anonymous}::TestFlowAlpha>, detail::extend<{anonymous}::TestFlowAlpha, flow::milestone_base> >&&)::<lambda(std::index_sequence<Is ...>)> [with long unsigned int ...Is = {0}; std::index_sequence<Is ...> = std::integer_sequence<long unsigned int, 0>]"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::chunk_by<get_service, tuple<detail::extend<msg::test_service>, detail::extend<msg::test_service, msg::callback_impl<msg::message_base<sc::string_constant<char, 't', 'e', 's', 't', '_', 'm', 's', 'g'>, 2, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'i', 'd', '_', 'f', 'i', 'e', 'l', 'd'>, 0, 31, 24, unsigned int, 128, msg::equal_to_t<msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'i', 'd', '_', 'f', 'i', 'e', 'l', 'd'>, 0, 31, 24, unsigned int, 0, match::always_t<true> >, unsigned int, 128> >, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '1'>, 0, 15, 0, unsigned int, 0, match::always_t<true> >, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '2'>, 1, 23, 16, unsigned int, 0, match::always_t<true> >, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '3'>, 1, 15, 0, unsigned int, 0, match::always_t<true> > >, msg::extra_callback_args<>, sc::string_constant<char, 'T', 'e', 's', 't', 'C', 'a', 'l', 'l', 'b', 'a', 'c', 'k'>, match::always_t<true>, msg::<lambda(const msg::test_msg_t&)> > > > >(tuple<detail::extend<msg::test_service>, detail::extend<msg::test_service, msg::callback_impl<msg::message_base<sc::string_constant<char, 't', 'e', 's', 't', '_', 'm', 's', 'g'>, 2, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'i', 'd', '_', 'f', 'i', 'e', 'l', 'd'>, 0, 31, 24, unsigned int, 128, msg::equal_to_t<msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'i', 'd', '_', 'f', 'i', 'e', 'l', 'd'>, 0, 31, 24, unsigned int, 0, match::always_t<true> >, unsigned int, 128> >, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '1'>, 0, 15, 0, unsigned int, 0, match::always_t<true> >, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '2'>, 1, 23, 16, unsigned int, 0, match::always_t<true> >, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '3'>, 1, 15, 0, unsigned int, 0, match::always_t<true> > >, msg::extra_callback_args<>, sc::string_constant<char, 'T', 'e', 's', 't', 'C', 'a', 'l', 'l', 'b', 'a', 'c', 'k'>, match::always_t<true>, msg::<lambda(const msg::test_msg_t&)> > > >&&)::<lambda(std::index_sequence<Is ...>)> [with long unsigned int ...Is = {0}; std::index_sequence<Is ...> = std::integer_sequence<long unsigned int, 0>]"))
  ;; types
  (should (tspew-test-run-in-buffer #'tspew--parse-type "constexpr const auto cib::initialized_builders<SimpleConfig>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "constexpr const auto cib::initialized<SimpleConfig, TestCallback<0> >::value"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "struct cib::initialized<SimpleConfig, TestCallback<0> >"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "struct cib::initialized<{anonymous}::SingleFlowSingleActionConfig, {anonymous}::TestFlowAlpha>"))
)

;; running Clang on CIB
(ert-deftest tspew-parse-cib-clang ()
  "Tests extracted from running on Intel compile-time-init-build using Clang"
  ;; functions
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::detail::tuple_impl<std::integer_sequence<unsigned long, 0, 1, 2, 3, 4, 5>, cib::detail::index_function_list<>, int, int, int, double, double, bool>::operator[]<18446744073709551615UL>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::detail::tuple_impl<std::integer_sequence<unsigned long, 0, 1, 2, 3, 4, 5>, cib::detail::index_function_list<>, int, int, int, double, double, bool>::operator[]<18446744073709551615UL>"))

  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::chunk_by(const cib::tuple<int, int, int, double, double, bool> &)::(anonymous class)::operator()(std::index_sequence<0UL, 1UL, 2UL>)::(anonymous class)::operator()<0UL, 1UL, 2UL>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::chunk_by(const cib::tuple<int, int, int, double, double, bool> &)::(anonymous class)::operator()<0UL, 1UL, 2UL>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::detail::tuple_impl<std::integer_sequence<unsigned long, 0, 1>, cib::detail::index_function_list<>, cib::detail::extend<TestCallback<0>>, cib::detail::extend<TestCallback<0>, SimpleConfig::(lambda at /home/jet/oss/compile-time-init-build/test/cib/nexus.cpp:22:38)>>::operator[]<18446744073709551615UL>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::detail::tuple_impl<std::integer_sequence<unsigned long, 0, 1>, cib::detail::index_function_list<>, cib::detail::extend<say_message>, cib::detail::extend<say_message, say_hello_world::(lambda at /home/jet/oss/compile-time-init-build/test/cib/readme_hello_world.cpp:18:34)>>::operator[]<18446744073709551615UL>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-function "cib::detail::tuple_impl<std::integer_sequence<unsigned long, 0, 1>, cib::detail::index_function_list<>, cib::detail::extend<msg::test_service>, cib::detail::extend<msg::test_service, msg::callback_impl<msg::message_base<sc::string_constant<char, 't', 'e', 's', 't', '_', 'm', 's', 'g'>, 2, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'i', 'd', '_', 'f', 'i', 'e', 'l', 'd'>, 0, 31, 24, unsigned int, 128, msg::equal_to_t<msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'i', 'd', '_', 'f', 'i', 'e', 'l', 'd'>, 0, 31, 24>, unsigned int, 128>>, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '1'>, 0, 15, 0>, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '2'>, 1, 23, 16>, msg::field<sc::string_constant<char, 't', 'e', 's', 't', '_', 'f', 'i', 'e', 'l', 'd', '_', '3'>, 1, 15, 0>>, msg::extra_callback_args<>, sc::string_constant<char, 'T', 'e', 's', 't', 'C', 'a', 'l', 'l', 'b', 'a', 'c', 'k'>, match::always_t<true>, msg::(lambda at /home/jet/oss/compile-time-init-build/test/msg/handler_builder.cpp:27:5)>>>::operator[]<18446744073709551615UL>"))

  ;; types
  (should (tspew-test-run-in-buffer #'tspew--parse-type "cib::chunk_by<std::type_identity_t, const cib::tuple<int, int, int, double, double, bool> &>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "cib::chunk<const cib::tuple<int, int, int, double, double, bool> &>"))
  (should (tspew-test-run-in-buffer #'tspew--parse-type "cib::chunk_by<cib::get_service, cib::tuple<cib::detail::extend<say_message>, cib::detail::extend<say_message, say_hello_world::(lambda at /home/jet/oss/compile-time-init-build/test/cib/readme_hello_world.cpp:18:34)>>>"))
  )
