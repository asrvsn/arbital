// // Build a route with an aynchronously loaded component
// const asyncRoute = ({store, path, componentPath, childRoutes}) => {
//   const props = {
//     path,
//     childRoutes,
//     getComponent (nextState, cb) {
//       /*  Webpack - use 'require.ensure' to create a split point
//           and embed an async module loader (jsonp) when bundling   */
//       require.ensure([], (require) => {
//         /*  Webpack - use require callback to define
//             dependencies for bundling   */
//         const component = require(componentPath).default(store)

//         /*  Return getComponent   */
//         cb(null, component)
//       })
//     }
//   }
//   return <Route {...props} />
// }

// const route = (props) => {
//   return (
//     <Route {...props} />
//   )
// }

// /*  Note: Instead of using JSX, we recommend using react-router
//     PlainRoute objects to build route definitions.   */

// const argumentsAPI = (store) => asyncRoute({
//   store,
//   path: 'arguments/:argumentid',
//   componentPath: '../components/Argument'
// })

// const claimsAPI = (store) => route({
//   path: 'claims',
//   childRoutes: [

//     asyncRoute({
//       store,
//       path: 'create',
//       componentPath: '../components/CreateClaim'
//     }),

//     asyncRoute({
//       store,
//       path: ':claimid',
//       componentPath: '../components/Claim',
//       childRoutes: [

//         asyncRoute({
//           store,
//           path: 'for',
//           componentPath: '../components/CreateArgument',
//         }),

//         asyncRoute({
//           store,
//           path: 'against',
//           componentPath: '../components/CreateArgument',
//         }),
//       ]
//     })
//   ]
// })

// const usersAPI = (store) => route({
//   path: 'users',
//   childRoutes: [

//     asyncRoute({
//       path: 'login',
//       componentPath: '../components/Login'
//     }),

//     asyncRoute({
//       path: 'logout',
//       componentPath: '../components/Logout'
//     }),

//     asyncRoute({
//       path: ':userid',
//       componentPath: '../components/User'
//     })
//   ]
// })

// export default (store) => route({
//   path: '/',
//   component: <Feed store={store} />,
//   childRoutes: [
//     argumentsAPI(store),
//     claimsAPI(store),
//     usersAPI(store)
//   ]
// })
