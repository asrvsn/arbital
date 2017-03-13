import React, { Component, PropTypes } from 'react'
import { connect } from 'react-redux'

import { Route, IndexRoute } from 'react-router'

import Feed from '../components/Feed'
import Argument from '../components/Argument'
import Claim from '../components/Claim'
import CreateArgument from '../components/CreateArgument'
import CreateClaim from '../components/CreateClaim'
import Login from '../components/Login'
import Logout from '../components/Logout'
import User from '../components/User'

const Routes = (props) => {
  const { isAuthenticated } = props

  const requireAuth = (nextState, replaceState) => {
    if (! isAuthenticated) {
      replaceState({ nextPathname: nextState.location.pathname }, '/login')
    }
  }

  return (
    <Route path='/' component={Feed} onEnter={requireAuth}>

      <Route path='login' component={Login} />
      <Route path='logout' component={Logout} onEnter={requireAuth} />

      <Route path='arguments'>
        <Route path=':argumentid' component={Argument} onEnter={requireAuth} />
      </Route>

      <Route path='claims'>
        <Route path=':claimid'>
          <IndexRoute component={Claim} onEnter={requireAuth} />
          <Route path='for' component={CreateArgument} onEnter={requireAuth} />
          <Route path='against' component={CreateArgument} onEnter={requireAuth} />
        </Route>
        <Route path='create' component={CreateClaim} onEnter={requireAuth} />
      </Route>

      <Route path='users'>
        <Route path=':userid' component={User} onEnter={requireAuth} />
      </Route>

    </Route>
  )
}

const mapStateToProps = ({ isAuthenticated }) => ({ isAuthenticated })
const mapDispatchToProps = (dispatch) => ({})

export default connect(
  mapStateToProps,
  mapDispatchToProps
)(Routes)
