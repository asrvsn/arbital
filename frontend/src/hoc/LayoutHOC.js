import React, { Component, PropTypes } from 'react'

import Header from '../components/Header'
import Footer from '../components/Footer'

const LayoutHOC = (ChildComponent) => {
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

export default LayoutHOC
