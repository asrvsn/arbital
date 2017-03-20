import React, { Component, PropTypes } from 'react'

import Header from '../Header'
import Footer from '../Footer'

const PageHOC = (ChildComponent) => {
  return (props) => {
    const { isAuthenticated } = props

    return (
      <div>
        <Header {...props} />
        <ChildComponent {...props} />
        <Footer {...props} />
      </div>
    )
  }
}

export default PageHOC
